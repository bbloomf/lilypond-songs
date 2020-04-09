\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Bonnie Doon"}}
  poet = \markup\oldStyleNum"Robert Burns (1759–1796)"
  composer = \markup\oldStyleNum{"Scotch Air," \italic"The Caledonian Hunt’s Delight"}
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
       (padding . -3)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 80))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.95\in
  outer-margin = 0.7\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #34
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
  \key a \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \mergeDifferentlyDottedOn
}

sopMusic = \relative c' {
	\partial 8
  e8 |
  a4 a8 b[ a] b |
  cis16[ e8.] cis8 b[ a] b |
  cis8.[ b16] a8 a16[ fis8.] e8 |
  e8.[ fis16] a8 b4\fermata \bar""\break e,8 |
  a4 a8 b[ a] b |
  
  cis8[ e] cis b[ a] b |
  cis8.[ b16] a8 a16[ fis8.] e8 |
  e[ fis] a a4\fermata \bar""\break cis16[ d] |
  e4 fis8 e16[ cis8.] a8 |
  e'4 fis8 e[ cis] a |
  e'[ cis] a e'[ cis] a |
  
  fis'8.[ e16] cis8 \acciaccatura cis8 b4\fermata \bar""\break e,8 |
  a4 a8 b[ a] b |
  cis8[ e] cis b[ a] b |
  cis8.[ b16] a8 a16[ fis8.] e8 |
  e[ fis] a a4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Ye banks and braes o’ bon -- nie Doon,
  How can ye bloom sae fresh and fair?
  How can ye chaunt, ye lit -- tle birds,
  And I sae wea -- ry, fu’ of care?
  Thou’lt break my heart, thou warb -- ling bird,
  That won -- tons through the flow -- ’ry thorn,
  Thou mindst me o’ de -- part -- ed joys,
  De -- part -- ed nev -- er to __ re -- turn.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Oft hae I rov’d by bon -- nie Doon,
  To see the rose and wood -- bine twine;
  When il -- ka bird sang o’ its love,
  And fond -- ly sae did I o’ mine.
  Wi’ light -- some heart I pu’d a rose,
  Fu’ sweet up -- on __ its thorn -- y tree;
  But my fause lov -- er stole my rose,
  And, ah! he left the thorn wi’ me.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 8
  e8 |
  e4 e8 gis[ fis] gis |
  a4 a8 gis8[ fis] gis |
  a8.[ gis16] a8 fis16[ d8.] cis8 |
  cis4 cis8 e4 e8 |
  cis4 cis8 e4 e8 |
  
  a4 e8 e4 e8 |
  e4 e8 fis16[ d8.] cis8 |
  cis8[ d] cis cis4 a'8 |
  a4 fis8 e16[ a8.] a8 |
  a4 fis8 e[ a] a |
  a4 a8 a4 a8 |
  
  fis8.[ a16] a8 gis4 e8 |
  cis4 cis8 e4 e8 |
  e4 e8 e4 e8 |
  e4 e8 fis16[ d8.] cis8 |
  cis8[ d] cis cis4 \bar"|."
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
  \partial 8
  cis8 |
  cis4 cis8 e4 e8 |
  e16[ cis8.] e8 e4 e8 |
  cis8.[ d16] cis8 d16[ a8.] a8 |
  a4 a8 gis4 gis8 |
  a4 a8 gis[ fis] gis |
  
  a[ cis] a gis4 gis8 |
  a8.[ b16] cis8 d16[ a8.] a8 |
  a4 a8 a4 a16[ b] |
  cis4 d8 cis16[ a8.] cis8 |
  cis4 d8 cis[ a] cis |
  cis[ e] cis cis[ e] cis |
  
  d8.[ cis16] a8 e'4 e,8 |
  a4 a8 gis4 gis8 |
  a[ cis] a gis[ fis] gis |
  a8.[ b16] cis8 d16[ a8.] a8 |
  a4 e8 e4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 8
  a8 |
  a4 a8 e4 e8 |
  a4 a8 e4 e8 |
  a4 a8 d,4 a8 |
  a4 a8 e'4\fermata e8 |
  a,4 a8 e'4 e8 |
  
  a4 a,8 e'4 e8 |
  a4 a8 d,4 a8 |
  a4 a8 a4 a'8 |
  a4 a8 a4 a8 |
  a4 a8 a4 a8 |
  a4 a8 a4 a8 |
  
  a4 a8 e4\fermata e8 |
  a,4 a8 e'4 e8 |
  a4 a8 e4 e8 |
  a4 a8 d,4 a'8 |
  a[ d,] a a4 \bar"|."
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
    \tempo 4 = 100
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


