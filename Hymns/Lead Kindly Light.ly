\version "2.14.2"
\include "../util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Lead Kindly Light"}}
  poet = \markup\oldStyleNum"John Henry Newman (1801–1890)"
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 0.75\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #132
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup{}
  evenHeaderMarkup = \markup {}
}
#(set-global-staff-size 20) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 20 20))) }
global = {
  \key aes \major
  \time 3/2
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 2.
  ees4 aes bes |
  c4. c8 bes4 aes f aes |
  f2( ees1) |
  aes2 g aes |
  bes2. \bar"||"
  ees,4 aes bes |
  
  c4. c8 bes4 aes f aes |
  f2( ees1) |
  ees2 aes2. g4 |
  g2( aes1) |
  bes2 bes bes |
  bes1 bes2 |
  
  bes4 c bes( aes) g( f) |
  ees2~ ees1 |
  c'2 bes aes |
  aes2. aes4 g f |
  ees2( aes2.) g4 |
  g2( aes4)\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = "1. "
	Lead, kind -- ly Light, a -- mid th’en -- cir -- cling gloom, __
  Lead Thou me on;
  The night is dark, and I am far from home, __
  Lead Thou me on. __
  
  Keep Thou my feet; I do not ask \set associatedVoice = "altos" to see __
  The dis -- tant scene; one step e -- nough __ for me. __
}

sopWordsII = \lyricmode {
  \set stanza = "2. "
  I was not ev -- er thus, nor prayed that Thou __
  Shouldst lead me on;
  I loved to choose and see my path; but now __
  Lead Thou me on. __
  
  I loved the gar -- ish day; and, spite \set associatedVoice = "altos" of fears, __
  Pride ruled my will: re -- mem -- ber not __ past years. __
}

sopWordsIII = \lyricmode {
  \set stanza = "3. "
  So long Thy pow’r has blest me, sure it still __
  Will lead me on
  O’er moor and fen, o’er crag and tor -- rent, till __
  The night is gone, __
  
  And with the morn those an -- gel fac -- \set associatedVoice = "altos" es smile, __
  Which I have loved long since, and lost __ a -- while. __
}

sopWordsIV = \lyricmode {
  \set stanza = "4. "
  Mean -- time, a -- long the nar -- row rug -- ged path, __
  Thy -- self hast trod,
  Lead, Sav -- iour, lead me home in child -- like faith, __
  Home to my God. __
\set associatedVoice = "bases"
  To rest for -- ev -- er af -- ter earth -- \set associatedVoice = "altos" ly strife __
  In the calm light of ev -- er -- last -- ing life. __
}

sopWordsV = \lyricmode {
  \set stanza = "5. "
}

altoMusic = \relative c' {
  ees4 ees ees |
  ees4. c8 des4 ees ees des |
  des2( ees1) |
  ees2 ees ees4( c) |
  des2. 
  ees4 ees ees |
  
  ees4. c8 des4 ees ees des |
  des2( c1) |
  c2 ees2. des4 |
  ees1. |
  ees2 d ees |
  f1 ees2 |
  
  ees4 ees d2 d |
  ees2( c des?) |
  c des ees |
  f2. f4 ees des |
  c2( ees2.) des4 |
  des2( c4) \bar"|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = "2. "
}
altoWordsIII = \lyricmode {
  \set stanza = "3. "
}
altoWordsIV = \lyricmode {
  \set stanza = "4. "
}
altoWordsV = \lyricmode {
  \set stanza = "5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = "6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  c4 c des |
  c4. aes8 aes4 aes aes aes |
  aes2( c des) |
  ees des c4( aes) |
  aes2( g4) 
  des' des des |
  
  c4. aes8 aes4 aes aes aes |
  aes2~ aes1 |
  aes2 c2. bes4 |
  des2( c1) |
  bes2 aes g4( c) |
  bes2( aes) g |
  
  g4 g aes2 aes |
  g( aes bes) |
  aes f4( g) aes2 |
  aes2. f4 g aes |
  aes2( c2.) bes4 |
  bes2( aes4) \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes4 aes aes |
  aes4. aes,8 bes4 c des f |
  aes2~ aes( bes2) |
  c bes aes |
  ees2. 
  ees4 f g |
  
  aes4. aes,8 bes4 c des f |
  aes2( aes, c) |
  ees ees2. ees4 |
  ees2( aes1) |
  g2 f ees |
  d1 ees2 |
  
  bes4 bes bes2 bes |
  ees1. |
  aes,2 bes c |
  des2. des4 des des |
  ees2~ ees2. ees4 |
  ees2( aes,4)\fermata \bar"|."
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
    \new Lyrics = "altos"
    \new Lyrics = "altosII"
    \new Lyrics = "altosIII"
    \new Lyrics = "altosIV"
    \new Lyrics = "altosV"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos"  \lyricsto "basses" \sopWords
    \context Lyrics = "altosII"  \lyricsto "basses" \sopWordsII
    \context Lyrics = "altosIII"  \lyricsto "basses" \sopWordsIII
    \context Lyrics = "altosIV"  \lyricsto "basses" \sopWordsIV
    \context Lyrics = "altosV"  \lyricsto "basses" \sopWordsV
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 105
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


