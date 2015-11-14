import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';
import { PropertyDetail } from './property.js';

export const Word = ({ word, workIndex, wordIndex, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const properties = R.addIndex(R.map)
    ((t, i) => (
      <PropertyDetail
        key={workIndex + '.' + wordIndex + '.' + i}
        name={getTypeTitle(t[0])}
        nameUrl={getValueListUrl(t[0])}
        valueIndexes={t[1]}
        getValue={x => getValueTitle(t[0], x)}
        getValueUrl={x => getInstanceListUrl(t[0], x)}
      />)
    )
    (word);
  return (
    <div className="wordContainer">
      {properties}
    </div>
  );
}
