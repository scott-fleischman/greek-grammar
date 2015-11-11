import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';

const Word = ({ word, key, getTypeTitle, getValueTitle }) => {
  const getTypeIndex = x => x[0];
  const getValueIndex = x => x[1];
  const getPropertyTitle = x => getTypeTitle(getTypeIndex(x));
  const getPropertyValue = x => getValueTitle(getTypeIndex(x), getValueIndex(x));

  const popoverList = R.addIndex(R.map) ((x, i) => (<div key={i}><strong>{getPropertyTitle(x)}</strong>: {getPropertyValue(x)}</div>)) (word);
  const popover = (
    <Popover id={'wordProperties.' + key} title="Properties" style={{maxWidth: '100%'}}>
      {popoverList}
    </Popover>
  );
  return (
    <OverlayTrigger trigger="click" rootClose placement="bottom" overlay={popover}>
      <span><a href="javascript:">{getPropertyValue(word[0])}</a> </span>
    </OverlayTrigger>
  );
}

const WordGroup = ({ key, words, getTypeTitle, getValueTitle }) => {
  const wordElements = R.map (x => (<Word key={key} {...x} />)) (words);
  return (
    <div className="wordGroup">
      {wordElements}
    </div>
  );
}

export const Work = ({ work, workIndex, getTypeTitle, getValueTitle }) => {
  const firstWord = work.words[0];
  const words = R.addIndex(R.map)
    ((w, i) => {
      const key = workIndex + '.' + i;
      return (
        <div key={key}>
          <Word word={w} key={key} getTypeTitle={getTypeTitle} getValueTitle={getValueTitle} />
        </div>
      );
    })
    (work.words);
  return (
    <div className="workContainer">
      { words }
    </div>
  );
};