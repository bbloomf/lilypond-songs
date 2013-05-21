\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Sole Mio"}}
  poet = \markup\oldStyleNum"Giovanni Capurro (1859–1920)"
  composer = \markup\oldStyleNum"Eduardo di Capua (1865–1917)"
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
  \key f \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 4.
  c'8 bes a |
  g4 f |
  f8 g a f |
  e4 d~ |
  d8 e f g |
  e8. d16 d4~ |
  d8 e f g |
  
  d8[ c] c4~ |
  c8 c' bes a |
  g4 f |
  f8 g a f |
  \acciaccatura f8 e4 d~ |
  d8 bes' a g |
  c a g f |
  g4. a8 |
  \times 2/3 {g16[ a g]} f4.~ |
  f8 f'-> f-> e-> |
  \acciaccatura e8 c4 c~ |
  c8 e e d |
  \acciaccatura d8 bes2~ |
  bes8 e e d |
  \acciaccatura d bes4 bes~ |
  bes8 g a bes |
  c2~ |
  c4 b8\rest c |
  des2~ |
  des8 bes f'8. des16 |
  c2~ |
  c8 a g f |
  c'2~ |
  c8 a \acciaccatura {g16[ a]} g8. e16 |
  f2~ |
  f8 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Be -- hold the bril -- liant sun in all its splen -- dor
  For -- got -- ten is the storm, the clouds now van -- ish.
  The fresh -- ’ning breez -- es, heav -- y airs will ban -- ish
  Be -- hold the bril -- liant sun in all its splen -- dor!
  
  \dropLyricsXII A sun I know of that’s bright -- er yet,
  This sun, my dear -- est ’tis naught but \raiseLyrics thee __
  Thy face, __ so fair to see, __
  That now my sun shall ev -- er be! __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Be -- hold the ra -- diant sun ’mid eve -- ning shad -- ows
  With gold -- en light it cov -- ers all cre -- a -- tion
  Un -- til it sinks be -- low the world’s foun -- da -- tion
  Be -- hold the ra -- diant sun ’mid eve -- ning shad -- ows!
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
  a'8 g f |
  e4 d |
  c8 c f c |
  c4 bes4~ |
  bes8 bes d d |
  bes8. bes16 bes4~ |
  bes8 c c e |
  
  d8[ c] c4~ |
  c8 a'8 g f |
  e4 d |
  c8 c f c |
  c4 bes4~ |
  bes8 d8 c e |
  f8 f e c |
  e4. c8 |
  c8 c4.~ |
  c8
  
  %Chorus
  a' a c |
  a4 a~ |
  a8 c c bes |
  g2~ |
  g8 c c bes |
  g4 e~ |
  e8 e f g |
  
  a2~ |
  a4 s8 a |
  bes2~ |
  bes8 bes des8. bes16 |
  a2~ |
  a8 f e c |
  e2~ |
  e8 c e8. c16 |
  c2~ |
  c8 \bar"|."
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
  a8 bes c |
  bes4 a |
  a8 bes c a |
  f4 f~ |
  f8 g bes bes |
  g8. bes16 f4~ |
  f8 g a bes |
  
  a4 a~ |
  a8 a8 bes c |
  bes4 a |
  a8 bes c a |
  f4 f~ |
  f8 g8 f bes |
  a c bes a |
  bes4. bes8 |
  bes8 a4.~ |
  a8
  
  %Chorus
  c8 c c |
  f,4 f~ |
  f8 a a f |
  e2~ |
  e8 g g e |
  e4 g~ |
  g8 bes c bes |
  
  a2~ |
  a4 s8 f |
  f2~ |
  f8 bes8 bes8. f16 |
  f2~ |
  f8 c' bes a |
  bes2~ |
  bes8 f bes8. bes16 |
  a2~ |
  a8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,8 f f |
  c4 d |
  f8 f f f |
  bes,4 bes~ |
  bes8 c bes g |
  c8. bes16 bes4~ |
  bes8 c c c |
  
  f4 f~ |
  f8 f f f |
  c4 d |
  f8 f f f |
  bes,4 bes~ |
  bes8 g a c |
  c c c c |
  c4. c8 |
  
  e8 f4.~ |
  f8 f f g |
  f4 f~ |
  f8 f f f |
  c2~ |
  c8 c c c |
  c4 c~ |
  c8 c f f |
  
  f2~ |
  f4 d8\rest f |
  bes,2~ |
  bes8 bes bes8. bes16 |
  f'2~ |
  f8 f f f |
  c2~ |
  c8 c c8. c16 |
  f2~ |
  f8 \bar"|."
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
    \tempo 4 = 60
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


