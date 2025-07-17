export const sendGenerationRequest = async (model, params, files) => {
    const formData = new FormData();

    const paramsBlob = new Blob([JSON.stringify(params)], {
        type: "application/json"
    });
    formData.append("params", paramsBlob);
    
    files.forEach((file, _) => {
        formData.append("files[]", file)
    });

    console.log(formData);
/*
    const resp = await fetch(`http://localhost:3000/${model.endpoint}`, {
        method: "POST",
        body: formData
    });

    if (!resp.ok)
        return null;
*/
    // Just return the full response for now.
    return "YAY";
};
