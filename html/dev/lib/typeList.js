import React from 'react';
import R from 'ramda';

const TypeInfo = ({ title, values, url }) => (
  <div>
    <a href={url}>{title}</a>
    &ensp;
    <span className="typeInfoCounts">{values.length} values, {R.compose(R.sum, R.map(x => x.i))(values)} instances</span>
  </div>
);

export const TypeList = ({ types, getTypeUrl }) => (
  <div className="listContainer">
    {R.addIndex(R.map) ((x, i) => (<TypeInfo key={i} {...x} url={getTypeUrl(i)} />)) (types)}
  </div>
);
