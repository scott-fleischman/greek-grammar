import React from 'react';
import R from 'ramda';

const TypeInfo = ({ title, valueCount, instanceCount, url }) => (
  <div>
    <a href={url}>{title}</a>
    &ensp;
    <span className="typeInfoCounts">{valueCount} values, {instanceCount} instances</span>
  </div>
);

export const TypeList = ({ types, getTypeUrl }) => (
  <div className="listContainer">
    {R.addIndex(R.map) ((x, i) => (<TypeInfo key={i} {...x} url={getTypeUrl(i)} />)) (types)}
  </div>
);
