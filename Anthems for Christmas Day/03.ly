\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"An Hymn for Christmas Day"}}
  %subtitle = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #12.5 \smallCapsOldStyle"Luke II."}}
  composer = \markup\oldStyleNum"William Knapp (1698–1768)"
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
       (padding . 2)
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
  \key g\major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  b'4 a g |
  d'2 d8[ c] |
  b([ a] g4) a8[ b] |
  c2 c4 |
  b2. \bar"||"

  fis4 g8[ a] b[ c] |
  d2 b4 |
  c8([ b] a4) a |
  a2. \bar"||"

  d4 d d |
  a( b) c |
  d2 d4 |
  d8[ c b c] d[ g,] |
  fis2. \bar"||"
  b8[ a] g[ fis] e4 |
  b'8.[ c16 b8 a] g4 |
  c8.([ d16] e4) d |
  b2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  Hark! hear you not a cheer -- ful, cheer -- ful Noise,
  which makes the Heav -- ens ring with Joys.

  See where light Stars bright An -- gels, An -- gels fly,
  a Thou -- sand Heav’n -- ly Ech -- oes cry.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  So loud they sung, that down to, down to Earth,
  in -- no cent Chil -- dren heard their Mirth;

  And sung with them what none could, none could say,
  for Joy their Prince was born that Day.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Their Prince, their God, like one of, one of those,
  is made a Child, and wrapt in Cloaths;

  All this in Time was ful -- ly, ful -- ly done,
  we have a Sav -- iour, God, the Son.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'4 fis e |
  d2 d4 |
  g2 fis4 |
  g8([ fis] e4) d |
  g2. \bar"||"

  d4 e8[ fis] g4 |
  a8.([ g16] fis4) g |
  e e2 |
  d2. \bar"||"

  d8[ e] fis4 g |
  a( g) fis |
  g8([ fis] e4) d |
  g8[ a b a] g4 |
  fis8.([ e16] d2) \bar"||"

  d4 e8[ fis] g[ a] |
  g2 d4 |
  g8[ b a g] fis4 |
  g2. \bar"|."
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
  r2. |
  d4 g, a |
  b( c) d |
  e8[ d c b] a[ g] |
  g2. \bar"||"

  a8[ b] c4 b |
  a( d) e |
  a,( d) cis |
  d2. \bar"||"

  r2. |
  d4 g c, |
  b( c) d |
  b8[ a g a] b[ c] |
  d2. \bar"||"

  b4 c8[ d] e[ d] |
  d2 g8[ fis] |
  e[ d c b] a[ g] |
  g2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 d e8[ fis] |
  g2 fis4 |
  g8([ fis] e4) d |
  c2 d4 |
  g,2. \bar"||"

  d'4 e8[ fis] g4 |
  d2 g4 |
  a2 a,4 |
  d2. \bar"||"

  b'8[ c] d4 g, |
  fis4( g) a |
  d,( e) fis |
  g8[ fis e fis] g[ a] |
  d,2. \bar"||"

  g8[ fis] e[ d] c[ b] |
  g'8.[ a16 g8 fis] e[ d] |
  c2 d4 |
  g,2. \bar"|."
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
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos" \lyricsto "sopranos" \sopWords
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
