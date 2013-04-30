\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Oh, happy is the man that hears"}}
  composer = \markup\oldStyleNum"Michael Bruce (1746–1767)"
  poet = \markup\oldStyleNum"George Frideric Handel (1685–1759)"
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
  \key ees \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  bes'4 |
  ees2 g,8.[ f16] |
  ees2 c'4 |
  aes4.( g8) aes4 |
  g2 bes4 |
  ees4.( f8) d[ c] |
  bes4.( c8) a4 |
  
  bes2\fermata \bar"" bes4
  ees2 g,8.[ f16] |
  ees2 c'4 |
  aes2 g4 |
  f2 bes4 |
  ees,2 aes4 |
  g4.( aes8) f4 |
  ees2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Oh, hap -- py is the man that hears
  In -- struc -- tion’s warn -- ing voice;
  And who ce -- les -- tial wis -- dom makes
  His ear -- ly, on -- ly choice.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  For she hath trea -- sures great -- er far
  Than east and west un -- fold;
  And her re -- wards more pre -- cious are
  Than all their stores of gold.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  She guides the young with in -- no -- cence
  In plea -- sure’s paths to tread;
  A crown of glo -- ry she be -- stows
  Up -- on the hoar -- y head.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Ac -- cord -- ing as her la -- bors rise,
  So her re -- wards in -- crease;
  Her ways are ways of plea -- sant -- ness,
  And all her paths are peace.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  ees4 |
  ees2 d4 |
  ees2 ees4 |
  ees2 d4 |
  ees2 f4 |
  g4.( f8) f[ g] |
  f2 ees4 |
  
  d2 g4 |
  g2 d4 |
  ees2 ees4 |
  ees( d) ees |
  d2 d4 |
  ees2 f4 |
  ees4.( f8) d4 |
  ees2 \bar"|."
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
  \partial 4
  g4 |
  g2 aes4 |
  g2 aes4 |
  c2 bes4 |
  bes2 bes4 |
  bes( a) bes8[ ees] |
  d4.( ees8) c4 |
  
  bes2 bes4 |
  bes2 aes4 |
  g2 aes4 |
  c( bes) bes |
  bes2 aes4 |
  g2 c4 |
  bes2 aes4 |
  g2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 |
  ees2 bes4 |
  c2 aes'4 |
  f2 bes,4 |
  ees2 d4 |
  c2 d8[ ees] |
  f2 f4 |
  
  bes,2\fermata ees4 |
  ees2 bes4 |
  c2 aes'4 |
  f2 ees4 |
  bes2 bes4 |
  c2 aes4 |
  bes2 bes4 |
  ees2 \bar"|."
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


