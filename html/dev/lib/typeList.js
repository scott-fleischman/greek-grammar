import React from 'react';
import R from 'ramda';
import { labelNumber } from './labelNumber.js';

const TypeInfo = ({ title, values, url }) => {
  const instanceCount = R.compose(R.sum, R.map(x => x.i))(values);
  return (
    <div>
      <a href={url}>{title}</a>
      &ensp;
      <span className="typeInfoCounts">{labelNumber(values.length, 'value', 'values')}, {labelNumber(instanceCount, 'instance', 'instances')}</span>
    </div>
  );
};

export const TypeList = ({ types, getTypeUrl }) => {
  const indexedTypes = R.addIndex(R.map) ((x, i) => ({ index: i, type: x })) (types);
  const orderedTypes = R.reverse(indexedTypes);
  return (
    <div className="listContainer">
      {R.map (x => (<TypeInfo key={x.index} {...x.type} url={getTypeUrl(x.index)} />)) (orderedTypes)}
    </div>
  );
};
