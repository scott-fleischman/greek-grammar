import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';

const Property = ({ name, value, url }) => (<div><strong>{name}</strong>: <a href={url}>{value}</a></div>);

export const Word = ({ word, workIndex, wordIndex, getTypeTitle, getValueTitle, getInstanceUrl }) => {
  const properties = R.addIndex(R.map)
    ((x, i) => (
      <Property
        key={i}
        name={getTypeTitle(x[0])}
        value={getValueTitle(x[0], x[1])}
        url={getInstanceUrl(x[0], x[1])}
      />))
    (word);
  return (
    <div className="wordContainer">
      {properties}
    </div>
  );
}
