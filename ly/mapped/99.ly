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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #99
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
  \tieDashed
  \slurDashed
}

sopMusic = \relative c' {
	\partial 4
  f4 |
  d f f bes8 a |
  g4 ees ees g8 g |
  
  f4 a c ees |
  d2. f,8 ees |
  d4 f f bes8 a |
  g4 ees ees b'8\rest g |
  
  f8 a c ees d4 c |
  bes2. f8 ees |
  d4 f f bes8 a |
  g4 ees ees g8 g |
  
  f4 a c ees |
  d2. f,8 ees |
  d4 f f bes8 a |
  g4 ees ees g8~ g |
  
  f8 a c ees d4 c8 c |
  bes2. b4\rest |
  
  %chorus
  d8 c4. bes g8 |
  f2. b4\rest |
  bes8 a4. bes d8 |
  
  c2. f,8 ees |
  d4 f f bes8 a |
  g4 ees ees g8 g |
  f8 ees'4. d c8 |
  bes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	I sit me down by my own fire -- side
  When the win -- ter nights come on,
  And I calm -- ly dream as the dim hours glide,
  Of ma -- ny plea -- sant scenes now gone;
  Of our health -- ful plays in my school -- boy days,
  That can nev -- er come a -- gain;
  Of our sum -- mer joys and our Christ -- mas toys,
  And ram -- bles o’er the stream -- let and plain.
  
  Hap -- py hours at home!
  Hap -- py hours at home!
  How the mo -- ments glide by the bright fire -- side,
  In the hap -- py hours at home.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  I sit me down by my own fire -- side
  Where the chil -- dren sport in glee,
  While the clear young voice of our house -- hold pride
  Makes mel -- o -- dy that’s dear to me.
  And by ev -- ’ry art that can charm the heart,
  They al -- lure my cares a -- way,
  To pre -- pare my soul as the swift hours roll,
  \set ignoreMelismata = ##t
  For the du -- ties of the bright com -- ing day.
  
  \set associatedVoice = "tenors"
  \skip1
  \dropLyricsXV
  Hap -- py hours at \raiseLyrics home!
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
  d4 |
  bes d f f8 f |
  ees4 bes bes ees8 ees |
  f4 f a a |
  bes2. d,8 c |
  
  bes4 d f f8 f |
  ees4 bes bes s8 ees |
  f8 f a a f4 a |
  bes2. d,8 c |
  
  bes4 d f f8 f |
  ees4 bes bes ees8 ees |
  f4 f a a |
  bes2. d,8 c |
  
  bes4 d f f8 f |
  ees4 bes bes ees8~ ees |
  f8 f a a f4 a8 a |
  bes2. s4 |
  
  %Chorus
  bes8 a4. g ees8 |
  d2. s4 |
  bes'8 fis4. g bes8 |
  
  a2. d,8 c |
  bes4 d f f8 f |
  g4 ees ees g8 g |
  f f4. f f8 |
  f2. \bar"|."
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
  bes4 |
  f bes d d8 d |
  bes4 g g bes8 bes |
  a4 c ees c |
  bes2. d8 ees |
  
  f4 f d d8 d |
  bes4 g g s8 bes |
  a c ees c f4 ees |
  d2. d8 ees |
  
  f4 f d d8 d |
  bes4 g g bes8 bes |
  a4 c ees c |
  bes2. d8 ees |
  
  f4 f d d8 d |
  bes4 g g bes8~ bes |
  a c ees c f4 ees8 ees |
  d2. s4 |
  
  %Chorus
  r1 |
  d,8 f4. bes c8 |
  d2. s8 g |
  
  f2. d8 ees |
  f4 f d d8 d |
  ees4 bes bes bes8 bes |
  a a4. f' ees8 |
  d2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  bes,4 |
  bes bes bes bes8 bes |
  ees4 ees ees ees8 ees |
  f4 f f f |
  bes,2. bes8 bes |
  
  bes4 bes bes bes8 bes |
  ees4 ees ees s8 ees |
  f f f f f4 f|
  bes,2. bes8 bes |
  
  bes4 bes bes bes8 bes |
  ees4 ees ees ees8 ees |
  f4 f f f |
  bes,2. bes8 bes |
  
  bes4 bes bes bes8 bes |
  ees4 ees ees ees8~ ees |
  f f f f f4 f8 f |
  bes,2. d4\rest |
  
  %Chorus
  bes8 bes4. ees ees8 |
  bes2. d4\rest |
  g,8 g4. c c8 |
  
  f,2. bes8 bes |
  bes4 bes bes bes8 bes |
  ees4 ees ees ees8 ees |
  f f4. f f8 |
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
    \new Lyrics = "altosII" 
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
    \new Lyrics = "tenors"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \context Lyrics = "tenors" \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Happy Hours at Home"}}
  composer = \markup\oldStyleNum"Stephen Foster (1826–1864)"
  tagline = ""
}}


