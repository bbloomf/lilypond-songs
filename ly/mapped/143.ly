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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #143
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
  \key bes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	g'2\mf bes4 a |
  g d' bes c |
  a2 b4\rest g\< |
  bes c\! d-> c |
  
  bes8[\> c] a4 g2 |
  b4\rest\!\< f g a |
  bes a bes c |
  d2\! b4\rest d\f |
  ees c d bes |
  
  c c bes2 |
  b4\rest f\p g f |
  bes a g a |
  bes2 b4\rest ees\mf |
  d c d bes |
  a8[ g] a4 g2 \bar"|."
}
sopWords = \lyricmode {
  When I in pain and sor -- row moan,
  And feel for -- sak -- en and a -- lone,
  ’Tis then I lift mine eyes on high
  To God, for help on Him re -- ly;
  And wait in pa -- tient pray’r be -- low,
  Un -- til His gra -- cious love He show.
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
  d2 g4 f |
  ees f g g |
  fis2 s4 d |
  g a bes a |
  
  g fis g2 |
  s4 d e f |
  f f d g |
  fis2 s4 g |
  g f? f d |
  
  g a f2 |
  s4 d ees d |
  g f ees4. ees8 |
  d2 s4 g |
  g g fis d |
  ees8[ g] fis4 d2 \bar"|."
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
  bes2 d4 d |
  bes bes bes ees |
  d2 s4 bes |
  d f f4. ees8 |
  
  d8[ ees] d4 bes2 |
  s4 a c c |
  d c bes g |
  a2 s4 bes |
  c8( bes4) a8 a4 bes |
  
  g c d2 |
  s4 bes bes4. bes8 |
  d4 d bes c |
  f,2 s4 c' |
  bes g a bes |
  c c b2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g2 g4 d |
  ees bes ees c |
  d2 d4\rest g |
  g f bes,-> f' |
  
  g8[ c,] d4 g2 |
  d4\rest d c f |
  bes, f' g ees |
  d2 d4\rest g |
  ees f d g |
  
  ees f bes2 |
  d,4\rest bes' ees, bes' |
  g d ees c |
  bes2 d4\rest c |
  g' ees d g |
  c,8[ ees] d4 g2 \bar"|."
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
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"When I in pain and sorrow moan"}}
  composer = \markup\oldStyleNum"Burkard Waldis (1490–1556)"
  arranger = \markup\oldStyleNum"Arranged by Michael Praetorius (1571–1621)"
  tagline = ""
}}


global = {
  \key a \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	cis'4 cis cis |
  b2 cis4 |
  d2 gis,4 |
  a2. |
  e4 e e |
  cis'2 b4 |
  b2. |
  
  d4 d cis |
  b2 a4 |
  gis2 fis4 |
  e2. |
  e4 fis a |
  cis2 b4 |
  a2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Je -- sus! the ver -- y thought of Thee
  With sweet -- ness fills my breast;
  But sweet -- er far Thy face to see,
  And in Thy pres -- ence rest.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Nor voice can sing, nor heart can frame,
  Nor can the mem -- ’ry find
  A sweet -- er sound than Thy blest name.
  O Sav -- ior of man -- kind!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  O hope of ev -- ’ry con -- trite heart!
  O joy of all the meek!
  To those who fall, how kind Thou art!
  How good to those who seek!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  e4 e e |
  fis2 e4 |
  d( fis) e |
  e2. |
  e4 e e |
  e2 dis4 |
  e2. |
  
  fis4 fis e |
  d2 dis4 |
  e2 dis4 |
  e2. |
  e4 e d |
  cis2 d4 |
  cis2. \bar"|."
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
  cis4 cis a |
  fis( b) ais |
  b2 b4 |
  cis2. |
  d4 cis b |
  a2 a4 |
  gis2. |
  
  fis4 gis ais |
  b2 b4 |
  b2 a4 |
  gis2. |
  a4 a a |
  a2 gis4 |
  a2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  a4 a cis, |
  d2 cis4 |
  b2 e4 |
  a2. |
  b4 a gis |
  a2 fis4 |
  e2. |
  
  b4 b b |
  b2 b4 |
  e2 e4 |
  e2( d4) |
  cis4 d fis |
  e2 e4 |
  a,2. \bar"|."
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
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Jesus! the very thought of Thee"}}
  composer = \markup\oldStyleNum"John Bacchus Dykes (1823–1876)"
  poet = \markup\oldStyleNum"Edward Caswall (1814–1878)"
  tagline = ""
}}


