\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Praise of Spring"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro"){ \abs-fontsize #12.5 "(Lob des Frühlings)"}}
  poet = \markup\oldStyleNum"Johann Ludwig Uhland (1787–1862)"
  composer = \markup\oldStyleNum"Felix Mendelssohn (1809–1847)"
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
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 67))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 67))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #4
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
  \key a \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c'' {
	\partial 2
  a4.\p b8 |
  cis4 r cis4.\cresc d8\! |
  e4 r e4.\sf fis8 |
  e d r4 d4.\sf e8 |
  cis4 r e4.\dim cis8 |
  
  b a r4 a4.\p gis8 |
  b2\fermata e,4\cresc e\! |
  fis4. fis8 d'[ cis] b[ a] |
  a4 gis gis gis |
  a4.\cresc a8\! fis'8[ e] d[ cis] |
  
  
  %page2
  cis4\sf d fis-. e-. |
  d-. cis-. b-. b8[ a] |
  gis2 e'4\p e |
  e4. a8 e[ cis] a[ cis] |
  e2 a,4^\markup{\dynamic"f" \italic"cresc."} cis |
  
  cis e8[ d] cis4. b8 |
  a2 e4.\p fis8 |
  gis4 r b4. a8 |
  gis4 r cis4.\sf b8 |
  b ais r4 d4. cis8 |
  
  
  %page3
  b4 r fis'4.\f e8 |
  e d r4 b4.\dim a8\! |
  gis4 r gis4.\p gis8 |
  gis2\fermata cis4\cresc eis,\! |
  fis4. fis8 d'[ cis] b[ a] |
  
  a4 gis gis\cresc gis\! |
  a4. a8 fis'[ e] d[ cis] |
  cis4 d fis e |
  d cis b b8[ a] |
  gis2 e'4\p e |
  
  %page4
  e4. a8 e[ cis] a[ cis] |
  e2 a,4\f cis |
  cis e8[ d] cis4. b8 |
  a2 cis4\p e |
  
  a, cis fis, a |
  dis,2 gis4 fis8[ e] |
  b'4^> a8[\cresc gis\!] d'4^> cis8[ b] |
  e2 cis4 e |
  
  %page5
  a1~ |
  a2 b,4 \once\override DynamicText #'self-alignment-X = #-1 fis'\mf |
  e4. cis8 cis4\dim b\! |
  a2\fermata
  \bar"|."
}
sopWords = \lyricmode {
	Op -- ’ning buds,
  black -- bird’s call,
  Lark’s sweet car -- ol,
  sun -- ny days,
  Fruit -- ful show -- ers,
  balm -- y gale!
  
  When of such sweet things we’re chant -- ing,
  Say, O Spring, what is there want -- ing
  Here on \dropLyricsXII earth to swell thy praise,
  \raiseLyrics here on earth to swell thy praise,
  \dropLyricsXII here on earth to swell thy \raiseLyrics praise?
  
  Op -- ’ning buds,
  black -- bird’s call,
  Lark’s sweet car -- ol,
  sun -- ny days,
  Fruit -- ful \dropLyricsXII show -- ers,
  balm -- y \raiseLyrics gale,
  balm -- y gale!
  
  \dropLyricsXII When of such sweet things we’re chant -- ing,
  Say, O Spring, what is there want -- ing
  Here on earth to swell thy praise, \raiseLyrics
  here on earth to swell thy praise,
  here on earth to swell thy praise,
  here on earth to swell thy praise,
  \set associatedVoice = "tenors" here on earth to swell thy praise,
  \set associatedVoice = "altos"
  here on earth, to swell thy praise,
  on earth to swell thy praise?
  
}
trueSopWords = \lyricmode {
  \repeat unfold 131 \skip 1
  \set stanza = \markup\dynamic"f " here on earth, __
  here
}

altoMusic = \relative c' {
  \partial 2
  e4. a8 |
  a4 r e4. fis8 |
  g4 r g4. g8 |
  fis fis r4 gis?4. gis8 |
  a4 r e4. e8 |
  
  e8 e r4 dis4. dis8 |
  e2\fermata e4 e |
  e d d8[ e] fis4 |
  e4. e8 d4 d |
  cis4. a'8 g4 g |
  
  %page2
  g4 fis fis-. fis-. |
  fis-. fis-. fis-. fis-. |
  e2 gis4 gis |
  a4. cis8 a[ e] cis[ e] |
  a2 a4 gis |
  
  fis fis a gis |
  a2 b,4. e8 |
  e4 r e4. e8 |
  e4 r g4. g8 |
  fis fis r4 ais4. ais8 |
  
  %page3
  b4 r b4. ais8 |
  ais b r4 fis4. fis8 |
  fis4 r fis4. fis8 |
  eis2\fermata eis4 cis |
  cis4. cis8 d[ e] fis4 |
  
  e4. e8 e4 e |
  e4. fis8 g4 g |
  g fis fis fis |
  fis fis fis fis |
  e2 gis4 gis |
  
  %page4
  a4. cis8 a[ e] cis[ e] |
  a2 a4 gis |
  fis fis a gis |
  a2 cis4 b |
  
  a gis fis e |
  dis2 d4. d8 |
  e4. e8 gis4. gis8 |
  a2 cis4 b |
  
  %page5
  a4 cis fis, a |
  dis,2. a'4 |
  a4. a8 gis4 gis |
  a2\fermata
  \bar"|."
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
  \dynamicDown
  \partial 2
  cis4.\p d8 |
  e4 r e4.\cresc d8\! |
  cis4 r cis4.\sf cis8 |
  d d r4 b4.\sf b8 |
  e4 r cis4.\dim cis8\! |
  
  cis cis r4 a4.\p a8 |
  gis2\fermata a4\cresc a\! |
  a4. a8 b[ cis] d4 |
  cis b b b |
  a4.\cresc cis8\! cis4 d8[ e] |
  
  %page2
  e4\sf d a-. ais-. |
  b-. e-. d-. c?-. |
  b2 d4 d |
  cis1~ |
  cis2 cis4_\markup{\dynamic"f" \italic"cresc."} b |
  
  a b e4. d8 |
  cis2 gis4.^\p a8 |
  b4 r d4. cis8 |
  b4 r e4.\sf d8 |
  cis8 cis r4 fis4. fis8 |
  
  %page3
  fis4 r d4.\f cis8 |
  cis b r4 d4.^\dim cis8\! |
  b1~ |
  b2\fermata b4\cresc b\! |
  a4. a8 b[ cis] d4 |
  
  cis4 b d\cresc d\! |
  cis4. d8 cis4 d8[ e] |
  e4 d ais b |
  b e d c |
  b2 d4\p d |
  
  %page4
  cis1~ |
  cis2 cis4\f b |
  a b e4. d8 |
  cis2 a'4\p gis |
  
  fis e d cis |
  b2 b4. b8 |
  d4^> cis8\cresc[ b\!] fis'4^> e8[ d] |
  cis2 a4\f b |
  
  %page5
  cis4 cis d e |
  fis2. b,4\mf |
  cis4. e8 e4\dim d\! |
  cis2\fermata
  \bar"|."
}
tenorWords = \lyricmode {
	\repeat unfold 43 \skip1
  
  swell thy praise, __
  "" \repeat unfold 6 \skip1
  
  \repeat unfold 19 \skip1
  gale! __
  
  "" \repeat unfold 22 \skip1
  swell thy praise, __
  "" \repeat unfold 20 \skip1
  %here on earth to swell thy praise,
  %on earth to swell thy praise?
}

bassMusic = \relative c' {
  \dynamicDown
  \partial 2
  a4. a8 |
  a4 r a4. a8 |
  a4 r a4. a8 |
  a a r4 a4. a8 |
  a4 r a4. a8
  
  fis8 fis r4 fis4. fis8 |
  e2 cis4 cis |
  d4. d8 d4 d |
  e e e e |
  fis fis a a |
  
  %page2
  a4 d, d-. d-. 
  d-. d-. d-. dis-. |
  e2 e4 e |
  a,1~ |
  a2 fis'4 eis |
  
  fis4 d e? e |
  a,2 r |
  e'4.\p e8 e2 |
  e4.\cresc e8\! e2 |
  e4. e8 e e r4 |
  
  %page3
  d4.\f d8 d4 fis |
  b8 b r4 r2 |
  r2 d,4.\p d8 |
  cis2\fermata cis4 cis |
  fis4. fis8 d4 d |
  
  e e e e\! |
  a4. a8 a4 a |
  a d, d d |
  d d d dis |
  e2 e4 e |
  
  %page4
  a,1~ |
  a2 fis'4 eis |
  fis d e? e |
  a1~ |
  a1~ |
  a2 a4. a8 |
  a4. a8 a4. a8 |
  a2 a4 gis |
  
  %page5
  fis4 e d cis |
  b2. dis4 |
  e4. e8 e4 e |
  a,2\fermata
  \bar"|."
}
bassWords = \lyricmode {
  \repeat unfold 53 \skip1
  Op -- ’ning buds,
  black -- bird’s call,
  Lark’s sweet car -- ol,
  Fruit -- ful, fruit -- ful show -- ers,
  balm -- y gale!
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
%{  <<
    \new Staff = "sopranos" \new Voice = "sopranos" { << \global \sopMusic >> }
    \new Staff = "altos" \new Voice = "altos" { << \global \altoMusic >> }
    \new Lyrics  \lyricsto "sopranos" \sopWords
    \new Lyrics \with { alignAboveContext = #"sopranos" } \lyricsto "sopranos" \trueSopWords
    \new Lyrics \with { alignBelowContext = #"altos" } \lyricsto "altos" \altoWords
    \new Staff = tenors { \clef "treble_8" \new Voice = "tenors" { << \global \tenorMusic >> } }
    \new Staff = basses { \clef bass \new Voice = "basses" { << \global \bassMusic >> } }
    
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"basses" } \lyricsto "basses" \bassWords
  >>
%}
  <<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "sop"
    \new Lyrics \with { alignAboveContext = #"women" } \lyricsto "sopranos" \trueSopWords
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"men" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"men" } \lyricsto "basses" \bassWords
    \context Lyrics = "sop" \lyricsto "sopranos" \sopWords
  >>
  >>
    %\new PianoStaff << \new Staff { \new Voice { \global \pianoRH } } \new Staff { \clef "bass" \global \pianoLH } >>
  \midi {
    \tempo 4 = 105
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


