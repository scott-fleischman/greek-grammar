import React from 'react';
import R from 'ramda';
import { labelNumber } from './labelNumber.js';

const ValueInfo = ({ title, instanceCount, url }) => (
  <div>
    <a href={url}>{title}</a>
    &ensp;
    <span className="valueInfoCount">{labelNumber(instanceCount, 'instance', 'instances')}</span>
  </div>
);

export const ValueList = ({ values, typeIndex, getInstanceListUrl, getShowItemInfo }) => {
  const { shownItems, showAllButton } = getShowItemInfo(2000, values);
  return (
    <div className="listContainer">
      {R.addIndex(R.map) ((x, i) => (
        <ValueInfo
          key={typeIndex + '.' + i}
          title={x.t}
          instanceCount={x.i}
          url={getInstanceListUrl(typeIndex, i)}
        />
      )) (shownItems)}

      {showAllButton}
    </div>
  );
};
