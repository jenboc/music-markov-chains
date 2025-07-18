import "../css/NumberSetting.css";
import { useState } from "react";

function NumberSetting({label, value, min, max, onChange}) {
    const [inputValue, setInputValue] = useState(min);
    
    const validateChange = e => {
        const newVal = parseInt(e.target.value);

        let val = newVal;
        if (min !== undefined && newVal < min)
            val = min;

        if (max !== undefined && newVal > max)
            val = max

        setInputValue(val);
        onChange(isNaN(val) ? min : val);
    };

    return <div className="number-setting">
        <label htmlFor={value} className="setting-label">
            <strong>{label}:</strong>
        </label>
        <input
            type="number"
            id={value}
            value={inputValue}
            className="setting-input"
            onChange={e => setInputValue(parseInt(e.target.value))}
            onBlur={validateChange}
        />
    </div>
}

export default NumberSetting;
