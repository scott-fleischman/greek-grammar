import React from 'react';
import R from 'ramda';
import { labelNumber } from './labelNumber.js';

const WorkInfo = ({ title, source, wordInfos, workUrl }) => (
  <div>
    <a href={workUrl}>{title}</a>
    &ensp;
    <span className="workListInfo">&mdash; {source} &mdash; {labelNumber(wordInfos.length, 'word', 'words')}</span>
  </div>
);

export const WorkList = ({ works, getWorkUrl }) => (
  <div className="listContainer">
    {R.addIndex(R.map) ((x, i) => (<WorkInfo key={i} {...x} workUrl={getWorkUrl(i)} />)) (works)}
  </div>
);
