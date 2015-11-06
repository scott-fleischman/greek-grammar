import React from 'react';
import R from 'ramda';

const WorkInfo = ({ title, source, wordCount, workIndex, workUrl }) => (
  <div>
    <span className="workInfoSource">{source}</span>
    &ensp;
    <a href={workUrl}>{title}</a>
    &ensp;
    <span className="workInfoWordCount">{wordCount} words</span>
  </div>
);

export const WorkList = ({ works, getWorkUrl }) => (
  <div className="workListContainer">
    {R.addIndex(R.map) ((x, i) => (<WorkInfo key={i} workIndex={i} {...x} workUrl={getWorkUrl(i)} />)) (works)}
  </div>
);
