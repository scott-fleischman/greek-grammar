import React from 'react';
import R from 'ramda';

const WorkInfo = ({ title, source, wordCount, workIndex, viewWork, getViewWorkUrl }) => (
  <div>
    <span className="workInfoSource">{source}</span>
    &ensp;
    <a href={getViewWorkUrl(workIndex)} onClick={() => viewWork(workIndex)}>{title}</a>
    &ensp;
    <span className="workInfoWordCount">{wordCount} words</span>
  </div>
);

export const WorkList = ({ works, viewWork, getViewWorkUrl }) => (
  <div className="workListContainer">
    {R.addIndex(R.map) ((x, i) => (<WorkInfo key={i} viewWork={viewWork} workIndex={i} {...x} getViewWorkUrl={getViewWorkUrl} />)) (works)}
  </div>
);
