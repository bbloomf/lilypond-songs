\version "2.14.2"
\include "util.ly"
\header {
  title = ""
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  ragged-last-bottom = ##t
  ragged-bottom = ##t
  two-sided = ##t
  ragged-right = ##f
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.5\in
  bottom-margin = 0.75\in
  first-page-number = #1
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = ""
  evenHeaderMarkup = ""
}
#(set-global-staff-size 23) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 23 20))) }
\markup\vspace #4
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #70 \smallCapsOldStyle"Songs"}}
\markup\vspace #0.75
\markup\fill-line \center-align {\abs-fontsize #35 \italic"from the"}
\markup\vspace #0.75
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #70 \smallCapsOldStyle"Public Domain"}}
\markup\vspace #10
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"selected, transcribed, and edited"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #24 \italic"by"}
\markup\vspace #0.25
\markup\fill-line \center-align {\abs-fontsize #28 \smallCapsOldStyle"benjamin bloomfield"}
\markup\vspace #9
\markup{\abs-fontsize #12 {First edition, \smallCapsOldStyle"august 2013"}}
\markup\vspace #0.1
\markup{\abs-fontsize #12 "This work is free of known copyright restrictions."}
\pageBreak
\markup\fill-line \center-align {\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #32 \smallCapsOldStyle"Contents"}}
\markup\vspace #2

%CONTENTS%
%\pageBreak
%\markup""