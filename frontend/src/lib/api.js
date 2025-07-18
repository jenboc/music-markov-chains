export const sendGenerationRequest = async (model, params, files) => {
    const formData = new FormData();

    formData.append("params", JSON.stringify(params));
    
    files.forEach((file, _) => {
        formData.append("files[]", file)
    });

    const resp = await fetch(`http://localhost:3000/${model.endpoint}`, {
        method: "POST",
        body: formData,
    });

    if (!resp.ok) {
        console.error("Generation Request error:", resp.statusText);
        return false;
    }

    // Get the file blob and create download URL
    const blob = await resp.blob();
    const url = window.URL.createObjectURL(blob);

    // Download the file
    const a = document.createElement("a");
    a.href = url;
    a.download = "generated.zip";
    document.body.appendChild(a);
    a.click();
    a.remove();

    setTimeout(() => URL.revokeObjectURL(url), 5000);

    return true;
};
