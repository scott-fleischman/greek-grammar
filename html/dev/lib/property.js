import React from 'react';
import R from 'ramda';
import { Panel } from 'react-bootstrap';

const Name = ({ name, nameUrl }) => (<a className="propertyName" href={nameUrl}>{name}</a>);
const Value = ({ value, valueUrl }) => (<a className="propertyValue" href={valueUrl}>{value}</a>);

export const SingleProperty = ({ name, nameUrl, valueIndexes, getValue, getValueUrl }) => (
  <div>
    <Name name={name} nameUrl={nameUrl} />
    <span className="propertySeparator">: </span>
    <Value value={getValue(valueIndexes[0])} valueUrl={getValueUrl(valueIndexes[0])} />
  </div>
);

export const PropertyDetail = ({ name, nameUrl, valueIndexes, getValue, getValueUrl }) => {
  const valueElements = R.addIndex(R.map)
    ((x, i) => (
      <div>
        <Value key={i} value={getValue(x)} valueUrl={getValueUrl(x)} />
      </div>
    ))
    (valueIndexes);
  return (
    <Panel header={(<Name name={name} nameUrl={nameUrl} />)}>
     {valueElements}
    </Panel>
  );
};
