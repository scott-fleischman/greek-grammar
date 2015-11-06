import React from 'react';
import R from 'ramda';

const ValueInfo = ({ title, instanceCount, url }) => (
  <div>
    <a href={url}>{title}</a>
    &ensp;
    <span className="valueInfoCount">{instanceCount} instances</span>
  </div>
);

export const ValueList = ({ values, typeIndex, getValueUrl }) => (
  <div className="listContainer">
    {R.addIndex(R.map) ((x, i) => (<ValueInfo key={i} title={x} instanceCount={0} url={getValueUrl(typeIndex, i)} />)) (values)}
  </div>
);
