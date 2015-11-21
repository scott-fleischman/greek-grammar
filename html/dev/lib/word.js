import React from 'react';
import R from 'ramda';
import { OverlayTrigger, Button, Popover, Panel } from 'react-bootstrap';
import { SingleProperty, PropertyDetail } from './property.js';

const PropertyList = ({ properties, workIndex, wordIndex, getTypeTitle, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const indexedProperties = R.addIndex(R.map) ((t, i) => ({ propertyIndex: i, typeIndex: t[0], valueIndexes: t[1] })) (properties);
  const propertyElements = R.map
    (x => (
      <PropertyDetail
        key={workIndex + '.' + wordIndex + '.property.' + x.propertyIndex}
        name={getTypeTitle(x.typeIndex)}
        nameUrl={getValueListUrl(x.typeIndex)}
        valueIndexes={x.valueIndexes}
        getValue={v => getValueTitle(x.typeIndex, v)}
        getValueUrl={v => getInstanceListUrl(x.typeIndex, v)}
      />)
    )
    (indexedProperties);
  return (
    <div>
      {propertyElements}
    </div>
  );
}

export const Word = ({ word, workIndex, wordIndex, stages, getTypeInfo, getValueTitle, getValueListUrl, getInstanceListUrl }) => {
  const getTypeTitle = x => getTypeInfo(x).title;
  const indexedStages = R.addIndex(R.map) ((x, i) => ({ index: i, stage: x })) (stages);
  const orderedStages = R.reverse(indexedStages);

  const wordPropertyElements = R.compose
    ( R.map((x, i) =>
        (<SingleProperty
          key={workIndex + '.' + wordIndex + '.wordProperty.' + x[0]}
          name={getTypeTitle(x[0])}
          nameUrl={getValueListUrl(x[0])}
          valueIndexes={x[1]}
          getValue={v => getValueTitle(x[0], v)}
          getValueUrl={v => getInstanceListUrl(x[0], v)}
        />)
      )
    , R.reverse
    , R.filter(x => getTypeInfo(x[0]).kind === 'Word Property')
    )
    (word);

  const stageElements = R.map
    (x => {
      const stageTypes = new Set(R.prepend(x.stage.primary, x.stage.parts));
      return (
        <Panel key={workIndex + '.' + wordIndex + '.stage.' + x.index} header={getTypeTitle(x.stage.primary)} bsStyle="info">
          <PropertyList
            properties={R.filter(w => stageTypes.has(w[0]))(word)}
            workIndex={workIndex}
            getTypeTitle={getTypeTitle}
            getValueTitle={getValueTitle}
            getValueListUrl={getValueListUrl}
            getInstanceListUrl={getInstanceListUrl}
          />
        </Panel>
      );
    })
    (orderedStages);

  return (
    <div className="wordContainer">
      <Panel header="Word Properties" bsStyle="primary">
        {wordPropertyElements}
      </Panel>

      {stageElements}
    </div>
  );
}
