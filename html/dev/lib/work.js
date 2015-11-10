import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover } from 'react-bootstrap';

const Word = ({ word, keyBase, wordIndex, getValue }) => {
  // const popoverList = R.map (x => (<div key={x}><strong>{propertyNames[x]}</strong>: {wordProps[x]}</div>)) (summaryProperties);
  const popover = (
    <Popover id={'wordProperties.' + keyBase + wordIndex} title="Properties" style={{maxWidth: '100%'}}>
      <div>Insert properties here</div>
    </Popover>
  );
  return (
    <OverlayTrigger trigger="click" rootClose placement="bottom" overlay={popover}>
      <span><a href="javascript:">{getValue(word[0][0], word[0][1])}</a> </span>
    </OverlayTrigger>
  );
}

// const WordGroup = ({id, words}) => {
//   const wordElements = R.map (x => (<Word key={x.key} {...x} />)) (words);
//   return (
//     <div className="wordGroup">
//       {wordElements}
//     </div>
//   );
// }

export const Work = ({ work, workIndex, getValue }) => {
  const firstWord = work.words[0];
  const keyBase = workIndex + '.';
  const words = R.addIndex(R.map)
    ((w, i) => (<div key={keyBase + '.' + i}><Word word={w} keyBase={keyBase} wordIndex={i} getValue={getValue} /></div>))
    (work.words);
  return (
    <div className="workContainer">
      { words }
    </div>
  );
};