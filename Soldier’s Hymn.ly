\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Soldier’s Hymn"}}
  composer = \markup\oldStyleNum"Franz Josef Haydn (1732–1809)"
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
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 0))
  markup-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #124
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
  \key aes \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	c'8.^\f des16 c8 c |
  des4 c |
  bes aes |
  bes8. c16 des bes c aes |
  
  c4 bes |
  c8. des16 c8 c |
  des4 c |
  bes aes |
  bes8. c16 aes8 g |
  
  bes4 aes8 b\rest |
  ees,8. f16 ees8 ees |
  f8. g16 f8 f |
  g8. aes16 g8 g |
  
  aes8 bes c4 |
  des8. ees16 des8 des |
  c8. des16 c8 c |
  bes8. c16 bes8 aes |
  
  aes4 g |
  c8.\< des16 c8 c |
  des4\ff c |
  bes aes |
  bes8.^\markup\italic"rall." c16 aes8 g |
  aes2 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	We, thy sol -- diers, hail thee, hail thee,
  Great Re -- pub -- lic, mo -- ther coun -- try;
  We thy sol -- diers hail thee, hail thee,
  On the eve of bat -- tle.
  Thou hast call’d us, “Arm ye, arm ye, O my brave and val -- iant sons.”
  Thou hast call’d us, “Arm ye, arm ye, Free -- dom is in per -- il.”
  We, thy sol -- diers, hail thee, hail thee:
  We go forth to war.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Forth to bat -- tle march we, march we,
  We, thy sons have heard the sum -- mons;
  Forth to bat -- tle march we, march we,
  We will fight for free -- dom.
  God of bat -- tles, be Thou with us,
  For our cause is just and right;
  God of bat -- tles, be Thou with us,
  Bring us home tri -- um -- phant!
  Forth to bat -- tle march we, march we,
  Na -- tion of the free.
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
  ees8. f16 ees8 ees |
  f4 ees |
  ees c |
  f8. f16 f f f f |
  
  ees4 ees |
  ees8. f16 ees8 ees |
  f4 ees |
  ees c |
  des8. des16 c8 bes |
  
  des4 c8 s |
  ees8. f16 ees8 ees |
  d8. d16 d8 d |
  des?8. des16 des8 ees |
  
  ees ees ees4 |
  des8. des16 des8 des |
  ees8. ees16 ees8 ees |
  f8. f16 f8 f |
  
  ees4 ees |
  ees8. f16 ees8 aes |
  aes4 aes |
  g f |
  f8. f16 ees8 ees |
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
  aes8. aes16 aes8 aes|
  aes4 aes |
  g aes |
  des8. c16 bes bes aes aes |
  
  aes4 g |
  aes8. aes16 aes8 aes |
  aes4 aes |
  g aes |
  f8. f16 ees8 ees |
  
  ees4 ees8 s |
  g8. aes16 g8 g |
  aes8. bes16 aes8 aes |
  bes8. c16 bes8 bes |
  
  aes g aes4 |
  aes8. aes16 aes8 aes |
  aes8. aes16 aes8 aes |
  des8. des16 des8 bes |
  
  c4 bes |
  aes8. aes16 aes8 aes |
  f'4 ees |
  ees c |
  des8. des16 c8 bes |
  c2 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes8. aes16 aes8 aes |
  des,4 aes' |
  ees f |
  des8. des16 des des d d |
  
  ees4 ees |
  aes8. aes16 aes8 aes |
  des,4 aes' |
  ees f |
  des8. bes16 ees8 ees |
  
  aes,4 aes8 d\rest |
  ees8. ees16 ees8 ees |
  ees8. ees16 ees8 ees |
  ees8. ees16 ees8 des |
  
  c ees aes4 |
  f8. f16 f8 f |
  ees8. ees16 ees8 ees |
  des8. des16 des8 d |
  
  ees4 ees |
  aes8. aes16 aes8 aes |
  des4 aes |
  ees f |
  des8. bes16 ees8 ees |
  aes,2 \bar"|."
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
    \tempo 4 = 45
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


