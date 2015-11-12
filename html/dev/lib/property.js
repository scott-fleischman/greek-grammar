import React from 'react';
import R from 'ramda';

export const Property = ({ name, value, nameUrl, valueUrl }) => (
  <div>
    <a className="propertyName" href={nameUrl}>{name}</a>
    <span className="propertySeparator">: </span>
    <a className="propertyValue" href={valueUrl}>{value}</a>
  </div>
);
