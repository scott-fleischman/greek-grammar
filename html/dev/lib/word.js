import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';
import { PropertyDetail } from './property.js';

export const Word = ({ word, workIndex, wordIndex, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const indexedProperties = R.addIndex(R.map) ((t, i) => ({ propertyIndex: i, typeIndex: t[0], valueIndexes: t[1] })) (word);
  const orderedProperties = R.reverse(indexedProperties);
  const propertyElements = R.map
    (x => (
      <PropertyDetail
        key={workIndex + '.' + wordIndex + '.' + x.propertyIndex}
        name={getTypeTitle(x.typeIndex)}
        nameUrl={getValueListUrl(x.typeIndex)}
        valueIndexes={x.valueIndexes}
        getValue={v => getValueTitle(x.typeIndex, v)}
        getValueUrl={v => getInstanceListUrl(x.typeIndex, v)}
      />)
    )
    (orderedProperties);
  return (
    <div className="wordContainer">
      {propertyElements}
    </div>
  );
}
