\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #33
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key a \major
  \time 3/4
  \autoBeamOff
  \override DynamicLineSpanner #'staff-padding = #0.0
  \override DynamicLineSpanner #'Y-extent = #'(-1 . 1)
}

sopMusic = \relative c' {
  \partial 4 e4^\p |
  a4 a cis8[ b] |
  a4 a e |
  fis a fis |
  e2 e4 |
  a a b |
  
  cis cis e |
  e cis a |
  b2 e,4 |
  a a cis8[ b] |
  a4 a e |
  fis d' fis, |
  
  e2 e4 |
  a a b |
  cis e\fermata d |
  e, e gis |
  a2\fermata gis8[^\markup\italic"piu mosso" a] |
  b4 b e |
  b b gis |
  
  b a fis |
  e2 gis8[ a] |
  b4 b e |
  b b gis |
  a8[ gis]^\markup\italic"poco rit." a[ b] cis[ dis] |
  << e2 {s4. s8^\f} >> fis4\fermata |
  << e4-> {s8 s8^\markup\italic"a tempo"}>> cis4 cis8[ b] |
  
  a4 a e |
  fis d' fis, |
  e2 e4^\p |
  a a b |
  cis^\pp e\fermata d |
  e, e gis |
  a2 \bar "|."
}
sopWords = \lyricmode {
  
}

altoMusic = \relative c' {
  e4 |
  e e e |
  e e cis |
  d fis d |
  cis2 cis4 |
  cis cis e |
  
  e e e |
  e e dis |
  e2 e4 |
  e e e |
  e e cis |
  d fis d |
  
  cis2 cis4 |
  cis e e |
  e e fis |
  e cis d |
  cis2 e8[ fis] |
  gis4 gis gis |
  gis gis e |
  
  dis dis dis |
  e2 e8[ fis] |
  gis4 gis gis |
  gis gis e |
  fis8[ e] fis[ gis] a4 |
  gis2 gis4 |
  a e e |
  
  e e cis |
  d fis d |
  cis2 cis4 |
  cis e e |
  e a fis |
  e cis d |
  cis2 \bar "|."
}
altoWords = \lyricmode {
  \set stanza = #"1. "
  Flow gent -- ly, sweet Af -- ton, a -- mong thy green braes;
  Flow gent -- ly, I’ll sing thee a song in thy praise;
  My Ma -- ry’s a -- sleep by thy mur -- mur -- ing stream,
  Flow gent -- ly, sweet Af -- ton, dis -- turb not her dream.
  Thou stock -- dove, whose ech -- o re -- sounds from the hill,
  Ye wild whist -- ling black -- birds in yon thorn -- y dell,
  Thou green -- crest -- ed lap -- wing, thy scream -- ing for -- bear,
  I charge you, dis -- turb not my slum -- ber -- ing fair.
}
altoWordsII = \lyricmode {
  \set stanza = #"2. "
  How loft -- y, sweet Af -- ton, thy neigh -- bor -- ing hills,
  Far marked with the cours -- es of \once \override LyricHyphen #'minimum-distance = #0.7 clear -- wind -- ing rills!
  There dai -- ly I wan -- der, as morn ris -- es high,
  My flocks and my Ma -- ry’s sweet cot in my eye.
  How pleas -- ant thy banks and green val -- leys be -- low,
  Where wild in the wood -- lands the prim -- ros -- es blow!
  There oft, as mild eve -- ning creeps o -- ver the lea,
  The sweet -- scent -- ed birk shades my Ma -- ry and me.
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
  Thy crys -- tal stream, Af -- ton, how love -- ly it glides,
  And winds by the cot where my Ma -- ry re -- sides!
  How wan -- ton thy wa -- ters her snow -- y feet lave,
  As, gath -- ’ring sweet flow -- ’rets, she stems thy clear wave!
  Flow gent -- ly, sweet Af -- ton, a -- mong thy green braes,
  Flow gent -- ly, sweet riv -- er, the theme of my lays;
  My Ma -- ry’s a -- sleep by the mur -- mur -- ing stream,
  Flow gent -- ly, sweet Af -- ton, dis -- turb not her dream.
}
altoWordsIV = \lyricmode {
}

tenorMusic = \relative c {
  e4_\p |
  cis' cis e8[ d] |
  cis4 cis a |
  a a a |
  a2 a4 |
  a a gis |
  
  a a cis |
  cis a a |
  gis2 e4 |
  cis' cis e8[ d] |
  cis4 cis a |
  a a a |
  
  a2 a4 |
  a a gis |
  a cis b |
  cis a b |
  a2 b4\rest |
  a1*3/4\rest |
  a1*3/4\rest |
  
  a1*3/4\rest |
  a2\rest b4\rest |
  a1*3/4\rest |
  a1*3/4\rest |
  a1*3/4\rest |
  << a2\rest {s4. s8_\f} >> d4 |
  cis-> a e'8[ d] |
  
  cis4 cis a |
  a a a |
  << a2 {s4. s8_\p} >> a4 |
  a a gis |
  a_\pp cis b |
  cis a b |
  a2 \bar "|."
}
tenorWords = \lyricmode {

}

bassMusic = \relative c {
  e4 |
  a, a a |
  a a a |
  d d d |
  a2 a4 |
  a cis e |
  
  a a a |
  a a fis |
  e2 e4 |
  a, a a |
  a a a |
  d d d |
  
  a2 a4 |
  a cis e |
  a a\fermata d, |
  e e e |
  a,2\fermata e'4 |
  e e e |
  e e e |
  
  b b b |
  e2 e4 |
  e e e |
  e e e |
  b b b |
  e2 e4\fermata |
  a,4 a a |
  
  a a a |
  d d d |
  a2 a4 |
  a cis e |
  a a\fermata d, |
  e e e |
  a,2 \bar "|."
}
bassWords = \lyricmode {

}

\score {
  <<
   \new ChoirStaff <<
%    \new Lyrics = sopranos \with { \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) }
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics \lyricsto "sopranos" \sopWords
    \new Lyrics \lyricsto "sopranos" \altoWords
    \new Lyrics \lyricsto "sopranos" \altoWordsII
    \new Lyrics \lyricsto "sopranos" \altoWordsIII
    \new Lyrics \lyricsto "sopranos" \altoWordsIV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" \override VerticalAxisGroup #'nonstaff-relatedstaff-spacing = #'((basic-distance . 1)) } \lyricsto "basses" \bassWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #1.3
    \override VerticalAxisGroup #'staff-affinity = #0
    \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 2)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 2)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Flow Gently, Sweet Afton"}}
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
  composer = \markup\oldStyleNum"Jonathan E. Spilman (1812–1896)"
  tagline = ""
}}


