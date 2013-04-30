\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"God so loved the world"}}
  composer = \markup\oldStyleNum"John Stainer (1840–1901)"
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
       (padding . -1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 80))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #134
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
  \key d \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\tempo \markup\italic"Andanta ma non lento"
  fis2.\p |
  fis4 e4. e8 |
  a2.~ |
  a4 b\rest b\rest |
  a2.\cresc |
  a4\~ gis4. gis8 |
  cis2.~ |
  cis4 b a |
  d2 cis4 |
  cis b a |
  
  g2 fis4 |
  b2
    b4\mf |
  e e d |
  cis d b |
  b a d, |
  d'2 b4\rest |
  d,2\p g4 |
  g-> fis2 |
  fis\cresc e'4\! |
  
  d cis b\f |
  a d fis |
  fis2 e4 |
  d2. |
  b4\rest fis\p fis |
  b a g |
  fis4. g8 fis e |
  d4\cresc fis b |
  
  d8. cis16 cis4 cis\mf |
  e e d |
  cis4. cis8 d b |
  ais4 b g |
  g8. fis16 fis2 |
  fis4\p fis fis |
  fis2 fis4 |
  
  fis cis' b |
  a2( gis4) fis2. |
  fis2.\pp |
  fis4 e4. e8 |
  a2.~ |
  a4 b\rest b\rest |
  a2.\cresc |
  a4\! gis4. gis8 |
  cis2.~ |
  cis4 b a |
  d2 cis4 |
  
  %page2
  cis4 b a |
  g2 fis4 |
  b2 b4\mf |
  e e d |
  cis d b |
  b a d, |
  d'2 b4\rest |
  d,2\p g4 |
  g-> fis2 |
  
  fis2\cresc e'4 |
  d cis b\f |
  a d fis |
  fis2 e4 |
  d2 f4\rest |
  f2.\rest |
  f4\rest d g |
  g2 fis4 |
  e2 d4 |
  
  cis2.(\dim |
  d2)^\markup\italic"rall." b4 |
  a2.\fermata |
  a\pp |
  b4 a4. a8 |
  d2.~ |
  d2 b4\rest |
  fis2. |
  fis4 e4. e8 |
  a2.~ |
  a2 b4\rest |
  fis2.\ppp |
  fis4^\markup\italic"rall." e fis |
  d2.\fermata \bar"|."
}
sopWords = \lyricmode {
  God so loved the world,
  God so loved the world that He gave His on -- ly be -- got -- ten Son,
  that who -- so be -- liev -- eth, be -- liev -- eth in Him
  should not per -- ish,
  should not per -- ish, but have ev -- er -- last -- ing life.
  For God sent not His Son in -- to the world to con -- demn the world,
  God sent not His Son in -- to the world to con -- demn the world;
  but that the world through Him might be sav -- ed.
  God so loved the world,
  God so loved the world that He gave His on -- ly be -- got -- ten Son,
  that who -- so be -- liev -- eth, be -- liev -- eth in Him
  should not per -- ish, should not per -- ish
  but have ev -- er -- last -- ing 
  \set associatedVoice = "altos"
  life,
  ev -- er -- last -- ing life, ev -- er -- last -- ing, ev -- er -- last -- ing
  \unset associatedVoice
  life,
  
  
  God so loved the world,
  God so loved the world,
  God so loved the world.
}

sopWordsII = \lyricmode {
}

sopWordsIII = \lyricmode {
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d2. |
  d4 cis4. cis8 |
  d2.~ |
  d4 s s |
  fis2. |
  fis4 eis4. eis8 |
  fis2.( |
  e4) d cis |
  d( fis) e |
  e d cis |
  
  b2 cis4 |
  d2 dis4 |
  e e fis |
  g g fis |
  e e fis |
  g2 s4 |
  d2 d4 |
  e e2 |
  cis fis4 |
  
  fis e d |
  fis a a |
  a2 g4 |
  fis2. |
  s4 d d |
  fis e d |
  cis4. d8 cis ais |
  b4 d fis |
  
  b8. ais16 ais4 fis |
  e e fis |
  g4. g8 fis fis |
  e4 fis d |
  e8. e16 e2 |
  fis4 fis fis |
  d( cis) b |
  
  cis cis d |
  cis2( b4) |
  a2. |
  d |
  d4 cis4. cis8 |
  d2.~ |
  d4 s s |
  fis2. |
  fis4 eis4. eis8 |
  fis2.( |
  e4) d cis |
  d( fis) e |
  
  %page 2 alto
  e4 d cis |
  b2 cis4 |
  d2 dis4 |
  e e fis |
  g g fis |
  e e fis |
  g2 s4 |
  d2 d4 |
  e e2 |
  
  cis2 fis4 |
  fis e d |
  fis a a |
  a2 g4 |
  fis fis\cresc b\! |
  b2 a4 |
  g g g |
  c2 c4 |
  b2 a4 |
  
  g2. |
  g |
  g |
  fis |
  e4 fis4. a8 |
  a2.( |
  g2) s4 |
  d2. |
  cis4 cis4. e8 |
  e2.( |
  d2) s4 |
  cis2. |
  cis4 cis cis |
  d2. \bar"|."
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
  a2. |
  a4 a4. a8 |
  a2.~ |
  a4 s s |
  cis2. |
  d4 d4. d8 |
  cis2. |
  a2 a4 |
  a2 ais4 |
  ais4 b cis |
  
  d2 cis4 |
  b2 b4 |
  b b b |
  ais b d |
  c c c |
  b2 s4 |
  d2 d4 |
  cis4 cis2 |
  e4( d) cis |
  
  b4 ais b |
  d fis d |
  d( b) cis |
  d2. |
  s4 b b |
  d cis b |
  ais4. b8 ais fis |
  b4 b d |
  
  fis8. fis,16 fis4 ais |
  b b b |
  ais4. ais8 b b |
  cis4 b d |
  cis8. cis16 cis2 |
  fis,4 fis fis |
  fis2 fis4 |
  
  fis fis fis |
  fis2( eis4) |
  fis2. |
  a |
  a4 a4. a8 |
  a2.~ |
  a4 s s |
  cis2. |
  d4 d4. d8 |
  cis2. |
  a2 a4 |
  a2 ais4 |
  
  %page2 tenor
  ais4 b cis |
  d2 cis4 |
  b2 b4 |
  b b b |
  ais b d |
  c c c |
  b2 s4 |
  d2 d4 |
  cis cis2 |
  
  e4( d) cis |
  b ais b |
  d fis d |
  d( b) cis |
  d d d |
  d( e) fis |
  g d d |
  dis2 dis4 |
  e2 fis4 |
  
  a,2. |
  b |
  e |
  c |
  c4 c4. c8 |
  b2.~ |
  b2 s4 |
  a2. |
  g4 g4. g8 |
  g2.( |
  fis2) s4 |
  a2. |
  a4 g a |
  fis2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,2. |
  g4 g4. g8 |
  fis2.~ fis4 d\rest d\rest |
  fis2. |
  b4 b4. b8 |
  a2. |
  g2 g4 |
  fis2 fis4 |
  g g a |
  
  b2 a4 |
  g2 a4 |
  g g fis |
  e d d |
  d d d |
  d2 d4\rest |
  d2 b'4 |
  ais4-> ais2 |
  ais2 ais4 |
  
  b4 fis g |
  a a a |
  a2 a4 |
  d2. |
  d,4\rest b2\rest |
  b2.\rest |
  b2.\rest |
  b2.\rest |
  b2\rest fis'4 |
  g g fis |
  e4. e8 d d |
  cis4 d b |
  ais8. ais16 ais2 |
  fis'4 fis fis |
  b,4( a?) gis |
  
  a a b |
  cis2. fis, |
  d' |
  g4 g4. g8 |
  fis2.~ |
  fis4 d\rest d\rest |
  fis2. |
  b4 b4. b8 |
  a2. |
  g2 g4 |
  fis2 fis4 |
  
  %page2 bass
  g4 g a |
  b2 a4 |
  g2 a4 |
  g g fis |
  e d d |
  d d d |
  d2 d4\rest |
  d2 b'4 |
  ais4-> ais2 |
  
  ais2 ais4 |
  b fis g |
  a a a |
  a2 a4 |
  b b b |
  c2 c4 |
  b b b |
  a2 a4 |
  g2 fis4 |
  
  e2. |
  d |
  cis\fermata |
  d |
  d4 d4. d8 |
  d2.~ |
  d2 d4\rest |
  d2. |
  d4 d4. d8 |
  d2.~ |
  d2 d4\rest |
  a2. |
  a4 a a |
  d2.\fermata \bar"|."
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


