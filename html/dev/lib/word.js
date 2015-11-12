import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';
import { Property } from './property.js';

export const Word = ({ word, workIndex, wordIndex, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const properties = R.addIndex(R.map)
    ((x, i) => (
      <Property
        key={i}
        name={getTypeTitle(x[0])}
        value={getValueTitle(x[0], x[1])}
        nameUrl={getValueListUrl(x[0])}
        valueUrl={getInstanceListUrl(x[0], x[1])}
      />))
    (word);
  return (
    <div className="wordContainer">
      {properties}
    </div>
  );
}
