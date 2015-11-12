import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';

const Word = ({ word, key, wordSummary, getTypeTitle, getValueTitle }) => {
  const getTypeIndex = x => x[0];
  const getValueIndex = x => x[1];
  const getPropertyTitle = x => getTypeTitle(getTypeIndex(x));
  const getPropertyValue = x => getValueTitle(getTypeIndex(x), getValueIndex(x));
  const summaryProperties = R.compose(R.sortBy(x => wordSummary.get(x[0])), R.filter(x => wordSummary.has(x[0]))) (word);

  const popoverList = R.addIndex(R.map)
    ((x, i) => (<div key={i}><strong>{getPropertyTitle(x)}</strong>: {getPropertyValue(x)}</div>))
    (summaryProperties);
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

const WordGroup = ({ key, wordIndexes, words, wordSummary, getTypeTitle, getValueTitle }) => {
  const wordElements = R.map
    (x => (<Word key={key + '.' + x} word={words[x]} wordSummary={wordSummary} getTypeTitle={getTypeTitle} getValueTitle={getValueTitle} />))
    (wordIndexes);
  return (
    <div className="wordGroup">
      {wordElements}
    </div>
  );
}

export const Work = ({ work, workIndex, getTypeTitle, getValueTitle }) => {
  const wordSummary = new Map(R.addIndex(R.map) ((x, i) => [x, i]) (work.wordSummary));
  const wordGroups = R.addIndex(R.map)
    ((wg, i) => (
      <WordGroup
        key={workIndex + '.' + i}
        wordIndexes={wg}
        words={work.words}
        wordSummary={wordSummary}
        getTypeTitle={getTypeTitle}
        getValueTitle={getValueTitle} 
      />))
    (work.wordGroups[0].words);
  return (
    <div className="workContainer">
      { wordGroups }
    </div>
  );
};
