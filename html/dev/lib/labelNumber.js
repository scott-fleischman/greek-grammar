export const labelNumber = (number, singular, plural) => {
  const word = number === 1 ? singular : plural;
  return `${number} ${word}`;
};
