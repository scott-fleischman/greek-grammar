import React from 'react';
import R from 'ramda';

const ValueInfo = ({ title, instanceCount, url }) => (
  <div>
    <a href={url}>{title}</a>
    &ensp;
    <span className="valueInfoCount">{instanceCount} instances</span>
  </div>
);

export const ValueList = ({ values, typeIndex, getInstanceListUrl }) => (
  <div className="listContainer">
    {R.addIndex(R.map) ((x, i) => (
      <ValueInfo
        key={typeIndex + '.' + i}
        title={x.t}
        instanceCount={x.i}
        url={getInstanceListUrl(typeIndex, i)}
      />
    )) (values)}
  </div>
);
