import { useState, useEffect } from "react";
import NumberSetting from "./NumberSetting";

function GenerationSettings({onChange}) {
    const [steps, setSteps] = useState(1);
    const [count, setCount] = useState(1);
    const [tpq, setTpq] = useState(100);

    useEffect(() => {
        onChange({
            "gen-steps": steps,
            "gen-count": count,
            "ticks-per-quarter": tpq
        });
    }, [steps, count, tpq]);

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
            <li>
                <strong>Ticks Per Quarter:</strong> The tempo of the generated midi
                file.
            </li>
        </ol>

        <NumberSetting
            label="Generation Steps"
            value="generation-steps"
            min={1}
            onChange={v => setSteps(v)}
        />

        <NumberSetting
            label="Generation Count"
            value="generation-count"
            min={1}
            onChange={v => setCount(v)}
        />

        <NumberSetting
            label="Ticks Per Quarter"
            value="ticks-per-quarter"
            min={100}
            onChange={v => setTpq(v)}
        />
    </div>
}

export default GenerationSettings;
