import { useRef, useState } from "react";

function FileUpload({ onUpload, accept="*/*", multiple=true }) {
    const fileInputRef = useRef(null);
    const [selectedFiles, setSelectedFiles] = useState([]);

    // Pass the file paths back to the parent
    const handleFileChange = (e) => {
        const files = Array.from(e.target.files || []);
        setSelectedFiles(files);
        onUpload(files);
    }

    // Trigger the file input
    const onButtonClick = () => {
        fileInputRef.current?.click();
    }

    return <div className="file-upload">
        <button type="button" onClick={onButtonClick} className="select-button">
            Select Files
        </button>
        <input
            ref={fileInputRef}
            type="file"
            style={{ display: "none" }}
            onChange={handleFileChange}
            accept={accept}
            multiple={multiple}
        />
        
        {selectedFiles.length > 0 && (
            <ul className="file-list">
                {selectedFiles.map((file, i) => (
                    <li key={i}>{file.name}</li>
                ))}
            </ul>
        )}
    </div>
}

export default FileUpload;
