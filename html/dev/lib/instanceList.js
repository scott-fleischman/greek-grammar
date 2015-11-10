import React from 'react';
import R from 'ramda';

const InstanceInfo = ({ getWorkInfo, getTypeInfo, workIndex, wordIndex, url }) => {
  const workInfo = getWorkInfo(workIndex);
  const wordTypes = workInfo.wordTypes;
  const wordInfo = workInfo.wordInfos[wordIndex];
  const wordValueIndexes = wordInfo.t;
  const wordContextIndex = wordInfo.c;
  const wordFile = wordInfo.f;
  const wordProperties = R.addIndex(R.map) ((vi, ti) => getTypeInfo(wordTypes[ti]).values[vi].t) (wordValueIndexes);
  return (
    <div>
      <a href={url}>{wordProperties[0]}</a>
      &ensp;
      <span className="instanceInfo">&mdash; {workInfo.source} &mdash; {workInfo.title} &mdash; {wordFile}</span>
    </div>
  );
};

export const InstanceList = ({ getWorkInfo, getTypeInfo, instances, typeIndex, valueIndex, getWordDetailUrl }) => {
  const getWorkIndex = x => x[0];
  const getWordIndex = x => x[1];
  return (
    <div className="listContainer">
      {R.addIndex(R.map) ((x, i) => (
        <InstanceInfo
          key={typeIndex + '.' + valueIndex + '.' + i}
          getWorkInfo={getWorkInfo}
          getTypeInfo={getTypeInfo}
          workIndex={getWorkIndex(x)}
          wordIndex={getWordIndex(x)}
          url={getWordDetailUrl(typeIndex, i)}
        />
      )) (instances)}
    </div>
  );
};
