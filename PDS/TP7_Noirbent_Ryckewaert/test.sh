#!/usr/bin/env bash

echo " -------------- Commandes AND --------------"

echo "./do -a true";
./do -a true;
echo $?;

echo "./do -a false";
./do -a false;
echo $?;

echo "./do -a true false";
./do -a true false;
echo $?;

echo "./do -a true true";
./do -a true true;
echo $?;

echo " -------------- Commandes OR --------------"
echo "./do -o true";
./do -o true;
echo $?;

echo "./do -o false";
./do -o false;
echo $?;

echo "./do -o true false";
./do -o true false;
echo $?;

echo "./do -o false false";
./do -o false false;
echo $?;


echo " -------------- Commandes AND CC --------------"

echo "./do -a -c true true false true";
./do -a -c true true false true;
echo $?;

echo "./do -a -c true true true";
./do -a -c true true true;
echo $?; 

echo "./do -a -c false true true";
./do -a -c false true true;
echo $?;

echo "./do -a -c false false false";
./do -a -c false false false;
echo $?;

echo " -------------- Commandes OR CC --------------"

echo "./do -o -c true true false true";
./do -o -c true true false true;
echo $?;

echo "./do -o -c true true true";
./do -o -c true true true;
echo $?; 

echo "./do -o -c false true true";
./do -o -c false true true;
echo $?;

echo "./do -o -c false false false";
./do -o -c false false false;
echo $?;
