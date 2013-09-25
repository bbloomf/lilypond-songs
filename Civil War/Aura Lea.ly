\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Aura Lea"}}
  %composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  %first-page-number = #196
  %print-first-page-number = ##t
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
#(set-global-staff-size 19) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 19 20))) }
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
  f4 bes a bes |
  c g c2 |
  bes4 a g a |
  bes1 |
  
  f4 bes a bes |
  c g c2 |
  bes4 a g a |
  bes1 |
  
  d4. d8 d2 |
  d4. d8 d2 |
  d4 c bes c |
  d2 b\rest |
  
  d4 d ees4. d8 |
  c4 g c4. bes8 |
  bes4 a g a |
  bes2 bes\rest \bar"||"\break
  

  d4. d8 d2 |
  d4. d8 d2 |
  d4 c bes c |
  d1 |
  
  d4 d ees4. d8 |
  c4 g c4. bes8 |
  bes4 a d4.\fermata c8 |
  bes1 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	When the black -- bird in the Spring,
  ’Neath the wil -- low tree,
  Sat and rocked, I heard him sing,
  Sing of Au -- ra Lea!
  
  
  Au -- ra Lea! Au -- ra Lea!
  Maid of gold -- en hair!
  Sun -- shine came a -- long with thee
  And swal -- lows in the air.


  Au -- ra Lea! Au -- ra Lea!
  Maid of gold -- en hair!
  Sun -- shine came a -- long with thee
  And swal -- lows in the air.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  In thy blush the rose was born,
  Mu -- sic when you spake,
  In thine a -- zure eye the morn
  Spark -- ling, seemed to break.

  Au -- ra Lea, Au -- ra Lea,
  Take my gold -- en ring,
  Love and life re -- turn with thee,
  And swal -- lows in the Spring
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
  f4 f f f |
  g g g2 |
  f4 f ees ees |
  d1 |
  
  f4 f f f |
  g f e2 |
  ees?4 ees ees ees |
  d1 |
  
  f4. f8 f2 |
  f4. f8 f2 |
  f4 a f a |
  bes2 s |
  
  f4 f ees4. f8 |
  g4 f e4. e8 |
  ees?4 ees4 ees4 ees |
  d2 s \bar"||"
  

  f4. f8 f2 |
  f4. f8 f2 |
  f4 f f f |
  f1 |

  f4 f g4. f8 |
  ees4 ees ees4. ees8 |
  d4 c8[ d] f4. ees8 |
  d1 \bar"|."
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
  d4 d d d |
  ees ees ees2 |
  ees4 ees c c |
  bes1 |
  
  d4 d d d |
  c b bes2 |
  g4 a bes c |
  bes1 |
  
  bes4. bes8 bes2 |
  bes4. bes8 bes2 |
  bes4 ees d ees |
  f2 s |
  
  bes,4 bes bes4. bes8 |
  c4 b4 bes4. g8 |
  f4 c' a a |
  f2 s \bar"||"
  

  bes4. bes8 bes2 |
  bes4. bes8 bes2 |
  bes4 a bes a |
  bes1 |

  bes4 bes b4. b8 |
  g4 g g4. g8 |
  f4 f f4. f8 |
  f1 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,4 bes bes bes |
  ees ees ees2 |
  f4 f f f |
  bes,1 |
  
  bes4 bes bes bes |
  ees d c2 |
  f4 f f f |
  bes,1 |
  
  bes4. bes8 bes2 |
  bes4. bes8 bes2 |
  bes4 f' bes, f' |
  bes,2 d\rest |
  
  bes'4 a g4. f8 |
  ees4 d4 c4. c8 |
  f4 f f f |
  bes,2 d\rest \bar"||"

  
  bes4. bes8 bes2 |
  bes4. bes8 bes2 |
  bes4 f' d f |
  bes,1 |

  bes4 bes g4. g8 |
  c4 c ees4. ees8 |
  f4 f f,4.\fermata f8 |
  bes1 \bar"|."
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
    \tempo 4 = 120
    \set Staff.midiInstrument = "flute"
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.5
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
