export const sendGenerationRequest = async (model, params, files) => {
    const formData = new FormData();

    console.log(params);

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
        return null;
    }

    // Get the file blob and create download URL
    const blob = await resp.blob();
    return blob;
};
