\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"We Sing the Praise of Him who Died"}}
  composer = \markup\oldStyleNum\concat{"From William Gardiner’s " \italic "Sacred Melodies" ", 1815"}
  poet = \markup\oldStyleNum"Thomas Kelly (1769–1854)"
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
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4 f4 |
  bes2 a4 |
  bes2 c4 |
  f,( g) a |
  bes2 \bar"" bes4 |
  
  bes4( a) g |
  c2 a4 |
  a8([ g] f4) e |
  f2 \bar""\break

  f4 |
  f2 bes4 |
  bes( a) g |
  f2 d'4 |
  d( c) \bar"" bes4 |
  a2 ees'4 |
  ees( d) c |
  bes( c) a |
  bes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  We sing the praise of Him who died,
  Of Him who died up -- on __ the Cross;
  The sin -- ner’s hope let men de -- ride,
  For this we count the world but loss.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  In -- scribed up -- on the Cross we see
  In shin -- ing let -- ters, ‘God is ‘love;’
  He bears our sins up -- on the tree;
  He brings us mer -- cy from a -- bove.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The Cross! it takes our guilt a -- way;
  It holds the faint -- ing spi -- rit up;
  It cheers with hope the gloom -- y day,
  And sweet -- ens ev -- ’ry bit -- ter cup.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  It makes the cow -- ard spi -- rit brave,
  And nerves the fee -- ble arm for fight;
  It takes the ter -- ror from the grave,
  And gilds the bed of death with light;
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  The balm of life, the cure of woe,
  The mea -- sure and the pledge of love,
  The sin -- ners’ ref -- uge here be -- low,
  The an -- gels’ theme in heav’n a -- bove.
}

altoMusic = \relative c' {
  d4 f2 ees4 |
  f2 c4 |
  ees2 ees4 |
  d2 d4 |
  e4( f) e |
  f2 f4 |
  d( c) bes |
  a2

  c4 |
  d2 d4 |
  ees2 ees4 |
  d2 f4 |
  g2 g4 |
  f2 a4 |
  bes2 g4 |
  f2 ees4 |
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
  bes4 d2 c4 |
  bes2 g4 |
  a( bes) c |
  bes2 bes4 |
  g4( c) c |
  c2 f,4 |
  bes( a) g |
  f2

  a4 |
  bes2 f4 |
  f4.( g8) a4 |
  bes2 bes4 |
  bes( ees) c |
  c2 c4 |
  bes2 ees4 |
  d( ees) c |
  bes2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,4 bes2 c4 |
  d2 ees4 |
  f2 fis4 |
  g2 g4 |
  c,2 bes4 |
  a2 d4 |
  bes( c) c |
  f2

  f4 |
  bes,2 bes4 |
  c2 c4 |
  d2 bes4 |
  ees2 e4 |
  f2 fis4 |
  g2 ees4 |
  f2 f4 |
  bes,2. \bar"|."
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
