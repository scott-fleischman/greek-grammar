import React from 'react';
import R from 'ramda';

const InstanceInfo = ({ words, workIndex, wordIndex, url }) => (
  <div>
    <a href={url}>Work: {workIndex}, Word: {wordIndex}</a>
  </div>
);

export const InstanceList = ({ words, instances, typeIndex, valueIndex, getWordDetailUrl }) => {
  const getWorkIndex = x => x[0];
  const getWordIndex = x => x[1];
  return (
    <div className="listContainer">
      {R.addIndex(R.map) ((x, i) => (
        <InstanceInfo
          key={typeIndex + '.' + valueIndex + '.' + i}
          workIndex={getWorkIndex(x)}
          wordIndex={getWordIndex(x)}
          url={getWordDetailUrl(typeIndex, i)}
        />
      )) (instances)}
    </div>
  );
};
