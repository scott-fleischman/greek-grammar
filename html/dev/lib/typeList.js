import React from 'react';
import R from 'ramda';
import { labelNumber } from './labelNumber.js';

const kindsToClassName = new Map([
  ['Word Stage', 'typeKindWordStage'],
  ['Word Stage Transition', 'typeKindWordStageTransition'],
  ['Word Stage Part Transition', 'typeKindWordStagePartTransition'],
  ['Word Stage Part', 'typeKindWordStagePart'],
  ['Word Property', 'typeKindWordProperty'],
  ['Work Property', 'typeKindWorkProperty'],
  ['Composite Property', 'typeKindCompositeProperty']
]);

const TypeInfo = ({ title, kind, values, url }) => {
  const instanceCount = R.compose(R.sum, R.map(x => x.i))(values);
  return (
    <div className={kindsToClassName.get(kind)}>
      <a href={url}>{title}</a>
      <span className="typeKind"> &mdash; {kind} &mdash; </span>
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
