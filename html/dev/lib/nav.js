import React from 'react';
import * as BS from 'react-bootstrap';

export const Nav = ({ title, viewWorkList, viewTypeList }) => {
  const onSelect = key => {
    if (key === 0)
      viewWorkList();
    else if (key === 1)
      viewTypeList();
    else
      console.log('Unknown nav key', key);
  };
  return (
    <BS.Navbar fluid>
      <BS.NavBrand>{title}</BS.NavBrand>
      <BS.Nav right onSelect={onSelect}>
        <BS.NavItem eventKey={0}>Works</BS.NavItem>
        <BS.NavItem eventKey={1}>Types</BS.NavItem>
      </BS.Nav>
    </BS.Navbar>
  );
};
