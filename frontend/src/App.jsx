import "./css/App.css";
import { useState, useEffect } from "react";
import { hasKeys } from "./lib/util"; 
import { sendGenerationRequest } from "./lib/api";
import ModelSelector from "./components/ModelSelector";
import ModelSettings from "./components/ModelSettings";
import FileUpload from "./components/FileUpload";
import GenerationSettings from "./components/GenerationSettings";
// import GeneratedFiles from "./components/GeneratedFiles";

function App() {
    const [model, setModel] = useState(null);
    const [files, setFiles] = useState([]);
    const [modelParams, setModelParams] = useState(null);
    const [genSettings, setGenSettings] = useState(null);
    const [warning, setWarning] = useState(null);
    const [error, setError] = useState(null);
    const [canSubmit, setCanSubmit] = useState(false);

    // When the form is updated, check it is valid
    useEffect(() => {
        // Settings have been changed so wipe the error
        setError(null);

        // Has a model been selected?
        if (model === null) {
            setWarning("No model is selected");
            setCanSubmit(false);
            return;
        }

        // Have we properly configured the model?
        let expectedKeys = model.params.map(p => p.value);
        if (modelParams === null || !hasKeys(modelParams, expectedKeys)) {
            setWarning("Please fully configure the model");
            setCanSubmit(false);
            return;
        }

        // Have we selected a proper amount of files?
        if (files.length === 0) {
            setWarning("No files are selected");
            setCanSubmit(false);
            return;
        }
        
        if (files.length > model.fileCap) {
            setWarning("File cap exceeded, you may proceed but performance issues may occur");
            setCanSubmit(true);
            return;
        }

        // Have we properly set generation settings?
        expectedKeys = ["gen-steps", "gen-count"];
        if (genSettings === null || !hasKeys(genSettings, expectedKeys)) {
            setWarning("Please fully configure generation");
            setCanSubmit(false);
            return;
        }

        // Everything is fine, remove the warning.
        setWarning(null);
        setCanSubmit(true);

    }, [model, files, modelParams, genSettings]);

    // Component Callbacks
    const handleModelSelection = e => setModel(e);
    const handleModelParamsChange = e => setModelParams(e);
    const handleUpload = (files) => setFiles(files);
    const handleGenSettingsChange = e => setGenSettings(e);

    const handleSubmit = () => {
        if (!canSubmit) {
            alert("Cannot submit the form. Please see the warning for more info.");
            return;
        }

        // Combine params and generation settings
        const params = {...modelParams, ...genSettings};

        // Send the request
        const resp = sendGenerationRequest(model, params, files);
    };

    return <div className="form-container">
        <h1>Music Generation Form</h1>

        <div className="form-section">
            <h2>Model Selection</h2>
            <p>
                Please select what model to use for generation.
            </p>
            <ModelSelector onSelect={handleModelSelection}/>
            <ModelSettings params={model?.params} onChange={handleModelParamsChange}/>
        </div>

        <div className="form-section">
            <h2>File Upload</h2>
            <p>
                Please select the midi files you would like to build the model from.<br/>
                <strong>Note:</strong> having a very large number of files may impact
                performance.
            </p>
            <FileUpload onUpload={handleUpload} accept=".mid,.midi" />
        </div>

        <div className="form-section">
            <h2>Generation Configuration</h2>
            <p>
                Please configure generation settings.
            </p>
            <GenerationSettings onChange={handleGenSettingsChange} />
        </div>

        {warning && <p className="warning-msg"><strong>Warning:</strong> {warning}</p>}
        {error && <p className="error-msg"><strong>Error:</strong> {error}</p>}

        <button type="button" onClick={handleSubmit}> 
            Submit
        </button>
    </div>
}

export default App;
