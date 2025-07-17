export const hasKeys = (obj, keyList) => {
    const objKeys = Object.keys(obj);
    return keyList.every(k => objKeys.includes(k));
};
