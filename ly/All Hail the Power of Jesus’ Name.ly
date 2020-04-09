\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"All Hail the Power of Jesus’ Name"}}
  composer = \markup\oldStyleNum"Edward Perronet (1721–1792)"
  poet = \markup\oldStyleNum"Oliver Holden (1765–1844)"
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
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 2 d2 |
  g4 g b b |
  a g a \bar"" b |
  a g b a |
  g2. \bar""\break
  
  a4 |
  b a g b |
  d8[ c] b[ a] b4 \bar""

  d4 |
  d2 d |
  e d4( cis) |
  d2. \bar""
  b4 |
  d b g b |
  a8[ g] a[ b] a4 \bar""
  g4 |
  d'2 c |
  b4.( c8 a4) a |
  g2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  All hail the pow’r of Je -- sus’ name!
  Let an -- gels pros -- trate fall.
  Bring forth the roy -- al di -- a -- dem,
  and crown Him Lord of __ all.
  Bring forth the roy -- al di -- a -- dem,
  and crown Him Lord __ of all!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  O seed of Is -- rael’s cho -- sen race
  now ran -- somed from the fall,
  Hail Him who saves you by His grace,
  and crown Him Lord of __ all.
  Hail Him who saves you by His grace,
  and crown Him Lord __ of all!
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Let ev -- ’ry tongue and ev -- ’ry tribe
  re -- spon -- sive to His call,
  To Him all maj -- es -- ty a -- scribe,
  and crown Him Lord of __ all.
  To Him all maj -- es -- ty a -- scribe,
  and crown Him Lord __ of all!
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Oh, that with all the sa -- cred throng
  we at His feet may fall!
  We’ll join the ev -- er -- last -- ing song
  and crown Him Lord of __ all.
  We’ll join the ev -- er -- last -- ing song
  and crown Him Lord __ of all!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  b2 |
  d4 d g g |
  fis e fis g |
  fis g d c |
  b2. d4 |
  g d b g' |
  b8[ a] g[ fis] g4

  fis4 |
  g2 a |
  g fis4( e) |
  fis2.

  g4 |
  g g g d |
  d d fis g |
  g2 e |
  d2. c4 |
  b2 \bar"|."
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
  g2 |
  b4 b d d |
  c b c d |
  c b g fis |
  g2. a4 |
  b a g b |
  d8[ c] b[ a] b4 

  a |
  b2 a |
  b a |
  a2. g4 |
  b d d d |
  c8[ b] c[ d] c4

  b4 |
  g2 g |
  g2. fis4 |
  g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g,2 |
  g4 g g' g |
  d e d g |
  d e d d |
  g,2. d'4 |
  g d b g' |
  b8[ a] g[ fis] g4

  d |
  g2 fis |
  e a |
  d,2. g4 |
  g g b g |
  d d d e |
  b2 c |
  d2. d4 |
  g,2 \bar"|."
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
