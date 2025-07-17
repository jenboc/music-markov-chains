export const naiveMaintainedModel = {
    endpoint: "generate/naiveMaintainedModel",
    fileCap: 100,
    params: [
        {
            label: "Degree",
            value: "degree",
            description: "Degree of the Markov Chain",
            min: 1
        }
    ]
};

export const naiveFlattenedModel = {
    endpoint: "generate/naiveFlattenedModel",
    fileCap: 100,
    params: [
        {
            label: "Degree",
            value: "degree",
            description: "Degree of the Markov Chain",
            min: 1
        }
    ]
};

export const complexModel = {
    endpoint: "generate/complexModel",
    fileCap: 100,
    params: [
        {
            label: "Duration Degree",
            value: "duration-degree",
            description: "Degree of the Duration Markov Chain",
            min: 1
        },
        {
            label: "Pitch Degree",
            value: "pitch-degree",
            description: "Degree of the Pitch Markov Chain",
            min: 1
        }
    ]
};
