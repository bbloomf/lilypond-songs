﻿\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Oh cease, my wandering soul"}}
  composer = \markup\oldStyleNum"John E. Gould (1820–1875)"
  poet = \markup\oldStyleNum"William Augustus Mühlenberg (1796–1877)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 1)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key e \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 e8[ fis] |
  gis4.( a8) gis4 |
  fis4.( gis8) fis4 |
  e2 \bar""

  b'8[ e] |
  dis4.( fis,8) a[ cis] |
  b4.( b,8) e[ gis] |
  fis2 \bar""\break

  gis8.[ e16] |
  cis2 a'8.[ fis16] |
  dis2 b'8.[ gis16] |
  e2 cis'4 |
  b2 \bar""

  bis4 |
  cis2 e4
  b4.( gis8) gis8.[ fis16] |
  e2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Like No -- ah’s wea -- ry dove,
  That soared the earth a -- round,
  But not a rest -- ing place a -- bove
  The cheer -- less wa -- ters found;
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oh, cease, my wan -- d’ring soul,
  On rest -- less wing to roam;
  All this wide world, to ei -- ther pole,
  Hath not for thee a home.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Be -- hold the ark of God!
  Be -- hold the o -- pen door!
  Oh, haste to gain that dear a -- bode,
  And rove, my soul, no more.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  There safe thou shalt a -- bide,
  There sweet shall be thy rest;
  And ev -- ’ry long -- ing sat -- is -- fied,
  With full sal -- va -- tion blest.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e4 |
  e2 e4 |
  dis2 dis4 |
  e2

  gis4 |
  a4.( dis,8) fis[ a] |
  gis4.( b,8) b[ e] |
  dis2

  e8.[ b16] |
  a2 fis'8.[ cis16] |
  b2 e4 |
  e2 a4 |
  gis2

  fis4 |
  e2 g4 |
  gis!4.( e8) dis4 |
  e2 \bar"|."
}

altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  gis8[ a] |
  b4.( cis8) b4 |
  a4.( b8) a4 |
  gis2

  b4 |
  b2 b8[ dis] |
  e4.( gis,8) b4 |
  b2

  b8.[ gis16] |
  e2 cis'8.[ a16] |
  fis2 b4 |
  gis2 e'4 |
  e2

  dis4 |
  cis( e) e |
  e4.( b8) b8.[ a16] |
  gis2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  e,4 |
  e2 e4 |
  b2 b4 |
  e2

  e4 |
  fis2 b,4 |
  e2 gis8[ e] |
  b2

  gis4 |
  a2 fis4 |
  b2 gis4 |
  cis2 a4 |
  e'2

  gis4 |
  a2 ais4 |
  b2 b,4 |
  e2 \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 90
    \set Staff.midiInstrument = "flute"
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
}
