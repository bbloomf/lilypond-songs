\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Auld Lang Syne"}}
  poet = \markup\oldStyleNum"First verse, traditional"
  meter = \markup\oldStyleNum"Other verses, Robert Burns (1759–1796)"
  composer = \markup\oldStyleNum"Traditional"
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
  first-page-number = #206
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
  \key g \major
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \partial 8 d8^\p |
  g8. g16 g8 b |
  a8. g16 a8 b |
  g8. g16 b8 d |
  e4. \bar""\break e8 |
  d8. b16 b8 g |
  
  a8. g16 a8 b |
  \slurDashed g8.( e16) e8( d) |
  g4 b8\rest \bar "|:" \break
  e8 |
  d8.[ b16] b8[ g] |
  a8. g16 a8 b16\rest b |
  
  b8.[ g16] b8.[ d16] |
  e4. \bar""\break e8 |
  d8. b16 b8 g |
  a8. g16 a8 b |
  g8.[ e16] e8[ d] |
  g4  b8\rest \bar ":|"
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
  Should auld ac -- quain -- tance be for -- got,
  And nev -- er brought to mind?
  Should auld ac -- quain -- tance be for -- got,
  And days of auld lang syne?

  \unset ignoreMelismata
  \set associatedVoice = "sopranos"
  For auld lang syne, my dear,
  For auld lang syne;
  We’ll tak’ a cup o’ kind -- ness yet
  For auld lang syne.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  We twa ha’e run a -- boot the braes,
  And pu’d the gow -- ans fine;
  But_we’ve wan -- der’d mon -- y~a wea -- ry foot,
  Sin’ auld __ lang __ syne.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  We twa ha’e sport -- ed i’ the burn,
  Frae morn -- in’ sun till dine,
  But seas be -- tween us braid ba’e roared
  Sin’ auld __ lang __ syne.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  And here’s a hand, my trust -- y frien’,
  And gie’s a hand o’ thine;
  We’ll tak’ a cup o’ kind -- ness yet,
  For auld __ lang __ syne.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d8 |
  d8. d16 d8 g |
  fis8. e16 fis8 fis |
  d8. d16 g8 g |
  g4. g8 |
  g8. g16 g8 g |
  
  fis8. e16 fis8 g |
  \slurDashed e8.( c16) c8( c) |
  b4 s8
  g'8 |
  g4 g |
  fis8. e16 fis8 s16 g |
  
  g8.[ e16] g8.[ b16] |
  c4. c8 |
  b8. g16 g8 g |
  fis8. e16 fis8 fis |
  e4 c4 |
  b4 s8 \bar ":|"
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
  b8 |
  b8. b16 b8 d |
  d8. d16 d8 d |
  b8. b16 d8 b |
  c4. c8 |
  b8. d16 d8 b |
  
  d8. d16 d8 d |
  \slurDashed c8.( g16) fis8( fis) |
  g4 s8
  c |
  b8.[ d16] d4 |
  d8. d16 d8 s16 d |
  
  d4 d |
  c4. c8 |
  d8. d16 d8 b |
  d8. d16 d8 d |
  b4 fis |
  g s8 \bar ":|"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g8 |
  g8. g16 g8 g |
  d8. d16 d8 d |
  g8. g16 g8 g |
  c,4. c8 |
  g'8. g16 g8 g |
  
  d8. d16 d8 g |
  \slurDashed c,8.( c16) d8( d) |
  g,4 d'8\rest
  g |
  g4 g4 |
  d8. d16 d8 d16\rest g |
  
  g4 g |
  c,4. c8 |
  g'8. g16 g8 g |
  d8. d16 d8 b |
  e4 d |
  g, d'8\rest \bar ":|"
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
      \new Voice = "sopranos" { \voiceOne << \global \set Staff.midiInstrument = #"flute" \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \set Staff.midiInstrument = #"flute" \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \set Staff.midiInstrument = #"flute" \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \set Staff.midiInstrument = #"flute"\bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 90
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
         (padding . 0.5)
         (stretchability . 2))
    }
  }
}
