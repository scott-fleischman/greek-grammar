import React from 'react';
import R from 'ramda';

const WorkInfo = ({ title, source, wordInfos, workUrl }) => (
  <div>
    <span className="workInfoSource">{source}</span>
    &ensp;
    <a href={workUrl}>{title}</a>
    &ensp;
    <span className="workInfoWordCount">{wordInfos.length} words</span>
  </div>
);

export const WorkList = ({ works, getWorkUrl }) => (
  <div className="listContainer">
    {R.addIndex(R.map) ((x, i) => (<WorkInfo key={i} {...x} workUrl={getWorkUrl(i)} />)) (works)}
  </div>
);
