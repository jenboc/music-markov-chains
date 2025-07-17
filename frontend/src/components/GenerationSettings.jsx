import { useState, useEffect } from "react";
import NumberSetting from "./NumberSetting";

function GenerationSettings({onChange}) {
    const [steps, setSteps] = useState(1);
    const [count, setCount] = useState(1);

    useEffect(() => {
        onChange({
            "gen-steps": steps,
            "gen-count": count
        });
    }, [steps, count]);

    return <div className="generation-settings">
        <p>You must set the following:</p>
        <ol>
            <li>
                <strong>Generation Steps:</strong> The number of steps to take
                during generation.
            </li>
            <li>
                <strong>Generation Count:</strong> The number of files to generate,
                each of which taking the above amount of steps.
            </li>
        </ol>

        <NumberSetting
            label="Generation Steps"
            value="generation-steps"
            min="1"
            onChange={e => setSteps(parseInt(e.target.value))}
        />

        <NumberSetting
            label="Generation Count"
            value="generation-count"
            min="1"
            onChange={e => setCount(parseInt(e.target.value))}
        />
    </div>
}

export default GenerationSettings;
