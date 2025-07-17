import { useState, useEffect } from "react";
import NumberSetting from "./NumberSetting";

function ModelSettings({params, onChange}) {
    const [settings, setSettings] = useState({});

    useEffect(() => {
        setSettings({});
    }, [params])

    const handleSettingChange = (k, v) => {
        const settingsClone = structuredClone(settings);
        settingsClone[k] = v;
        setSettings(settingsClone);
        onChange(settingsClone);
        console.log(settingsClone);
    };

    if (params === null || params === undefined) {
        return;
    }

    return <div className="model-settings">
        <p>You must set the following settings:</p>
        <ol>
            {params.map(param => (
                <li key={param.value}>
                    <strong>{param.label}:</strong> {param.description}
                </li>
            ))}
        </ol>
        
        {params.map(({label, value, min}) => (
            <NumberSetting 
                key={value}
                label={label}
                value={value}
                min={min}
                onChange={e => handleSettingChange(value, parseInt(e.target.value))} 
            />
        ))}
    </div>
}

export default ModelSettings;
