\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Long, Long Ago"}}
  composer = \markup\oldStyleNum"Thomas Haynes Bayly (1797–1839)"
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
       (stretchability . 150))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #60
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
  \key f \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	f4 f8 g a4 a8 bes |
  c4 d8 c a4 b4\rest |
  c bes8 a g4 b\rest |
  bes a8 g f4 b\rest |
  
  f4 f8 g a4 a8 bes |
  c4 d8 c a4 b\rest |
  c4 bes8 a g4 a8.\fermata g16 |
  f2 b\rest |
  
  c4 bes8 a g4 c,8 c |
  bes'4 a8 g f4 b\rest |
  c bes8 a g4 c,8 c |
  bes'4 a8 g f4 b\rest |
  
  f4 f8 g a4 a8 bes |
  c4 d8 c a4 b\rest |
  c\p bes8 a g4 a8.\fermata g16 |
  f2 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Tell me the tales that to me were so dear,
  Long, long a -- go,
  Long, long a -- go;
  
  Sing me the songs I de -- light -- ed to hear,
  Long, long a -- go, long a -- go.
  
  Now you are come, all my grief is re -- moved,
  Let me for -- get that so long you have roved,
  Let me be -- lieve that you love as you loved,
  Long, long a -- go, long -- a -- go.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Do you re -- mem -- ber the path where we met,
  Long, long a -- go,
  Long, long a -- go?
  
  Ah, yes, you told me you ne’er would for -- get,
  Long, long a -- go, long -- a -- go.
  
  Then, to all oth -- ers my smile you pre -- ferr’d,
  Love, when you spoke, gave a charm to each word,
  Still my heart  trea -- sures the prais -- es I heard,
  Long, long a -- go, long a -- go.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Though by your kind -- ness my fond hopes were raised,
  Long, long a -- go,
  Long, long a -- go;
  
  You by more el -- o -- quent lips have been praised,
  Long, long a -- go, long a -- go.
  
  But by long ab -- sence your truth has been tried,
  Still to your ac -- cents I lis -- ten with pride,
  Blest as I was when I sat by your side,
  Long, long a -- go, long a -- go.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  f4 f8 e f4 f8 g |
  a4 bes8 a f4 s |
  a g8 f e4 s |
  f f8 e f4 s |
  
  c c8 e f4 f8 g |
  a4 bes8 a f4 s |
  f f8 f e4 e8. e16 |
  f2 s |
  
  a4 g8 f e4 c8 c |
  e4 f8 e f4 s |
  a g8 f e4 c8 c |
  e4 f8 e f4 s |
  
  c4 c8 e f4 f8 g |
  a4 bes8 a f4 s |
  a4 g8 f e4 e8. e16 |
  f2 s \bar"|."
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
  a4 a8 c c4 c8 c |
  c4 c8 c c4 s |
  c4 c8 c c4 s |
  d4 c8 bes a4 s |
  
  a a8 c c4 c8 c |
  c4 c8 c c4 s |
  a d8 c bes4 c8. bes16 |
  a2 s |
  
  c4 c8 c c4 g8 g |
  c4 c8 bes a4 s |
  c4 c8 c c4 g8 g |
  c4 c8 c16[ bes] a4 s |
  
  a4 a8 c c4 c8 c |
  c4 c8 c c4 s |
  c c8 c c4 c8. bes16 |
  a2 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 f8 c f4 f8 f |
  f4 f8 f f4 d\rest |
  c e8 f c4 d\rest |
  c c8 c f4 d\rest |
  
  f f8 c c4 f8 f |
  f4 f8 f f4 d\rest |
  f f8 f c4 c8.\fermata c16 |
  f2 d\rest |
  
  c4 e8 f c4 e8 e |
  g4 f8 c f4 d\rest |
  f e8 f c4 e8 e |
  g4 f8 c f4 d\rest |
  
  f f8 c f4 f8 f |
  f4 f8 f f4 d\rest |
  f e8 f c4 c8.\fermata c16 |
  f2 d\rest \bar"|."
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


