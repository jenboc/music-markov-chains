import { useState } from "react";
import { naiveMaintainedModel, naiveFlattenedModel, complexModel } from "../lib/models";

const OPTIONS = [
    {
        value: "naive-maintained",
        label: "Naive Chain with Maintained Parallels",
        description: `Markov Chain using notes as they appear in the midi files,
            maintaining parallel structures, i.e. multiple notes playing at a given
            instant.`
    },
    {
        value: "naive-flattened",
        label: "Naive Chain with Flattened Parallels",
        description: `Markov Chain using notes as they appear in the midi files,
            flattening parallel structures, i.e. multiple notes playing at a given
            instant.`
    },
    {
        value: "complex",
        label: "Complex Model (Multiple Chains)",
        description: `The complex model creates two separate markov chains, one for
            note duration, and another for note pitch. At this time, generated pieces
            do not contain rests or parallel structures.`
    }
];

const valueToModel = value => {
    switch (value) {
        case "naive-maintained":
            return naiveMaintainedModel;

        case "naive-flattened":
            return naiveFlattenedModel;

        case "complex":
            return complexModel;

        default:
            return null;
    }
};

function ModelSelector({onSelect}) {
    const [placeholderDisabled, setPlaceholderDisabled] = useState(false);

    const onSelectChange = e => {
        setPlaceholderDisabled(true);
        onSelect(valueToModel(e.target.value));
    };

    return <div className="model-selection">
        <p>There are {OPTIONS.length} models to choose from.</p>
        <ol>
            {OPTIONS.map(opt => (
                <li key={opt.value}>
                    <strong>{opt.label}:</strong> {opt.description}
                </li>
            ))}
        </ol>

        <select onChange={onSelectChange}>
            {!placeholderDisabled && <option value="">
                -- Select Model --
            </option>}
            {OPTIONS.map(opt => (
                <option key={opt.value} value={opt.value}>
                    {opt.label}
                </option>
            ))}
        </select>
    </div>
}

export default ModelSelector;
