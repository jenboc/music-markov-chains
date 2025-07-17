function NumberSetting({label, value, min, onChange}) {
    return <div className="number-setting">
        <label htmlFor={value} className="setting-label">
            {label}
        </label>
        <input
            type="number"
            id={value}
            onChange={onChange}
            min={min === undefined ? 0 : min}
            className="setting-input"
        />
    </div>
}

export default NumberSetting;
