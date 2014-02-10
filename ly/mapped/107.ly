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
  first-page-number = #107
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
	\partial 4
  f4 |
  d'8. cis16 d4 ees |
  d8. cis16 c4 f, |
  c'8. b16 c4 d |
  
  \times 2/3 {c8[ bes] g} f4 \bar"" f |
  d'8. cis16 d4 f |
  f8. e16 ees4 ees |
  d8. cis16 d4 f,4 |
  
  c'8. bes16 bes4 \bar""\break b8\rest bes |
  c d ees4 g, |
  f8 bes d4 bes |
  c8 d f4 ees |
  
  bes8 c d4 bes8\rest \bar""\break d |
  d d ees4 d |
  c8 d g,4 a |
  bes8 bes c4 bes |
  
  %page 2
  a8 g d'8[\fermata ees]\fermata \bar"||"
  ees4\fermata |
  d8. cis16 d4 ees |
  d8. cis16 c4 f, |
  
  c'8. b16 c4 d |
  \times 2/3 {c8[ bes] g} f4 f |
  d'8. cis16 d4 f |
  
  f8 e ees4\fermata ees |
  d8. cis16 d4 f, |
  c'8. bes16 bes4 \bar"||"
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark \markup\italic"CODA ad lib." 
  
  ees4\fermata|
  d8. cis16 d4->\fermata \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	O Gen -- e -- vieve, I’d give the world
  To live a -- gain the love -- ly past!
  The rose of youth was dew -- im -- pearled,
  But now it with -- ers in the blast.
  
  I see thy face in ev -- ’ry dream,
  My wak -- ing thoughts are full of thee;
  Thy glance is in the star -- ry beam
  That falls a -- long the sum -- mer sea.
  
  O Gen -- e -- vieve, Sweet Gen -- e -- vieve,
  The days may come, the days may go,
  But still the hands of mem -- ’ry weave
  The bliss -- ful dreams of long a -- go.
  
  O Gen -- e -- vieve!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Fair Gen -- e -- vieve, my ear -- ly love,
  The years but make thee dear -- er far!
  My heart shall nev -- er, nev -- er rove:
  Thou art my on -- ly guid -- ing star.
  
  For me the past has no re -- gret,
  What -- e’er the years may bring to me;
  I bless the hour when first we met,
  The hour that gave me love and thee!
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
  f4 |
  f8. e16 f4 g |
  f8. e16 ees4 ees |
  
  ees8. d16 ees4 f |
  \times 2/3 {d8[ ees] ees} d4 d |
  f8. e16 f4 aes |
  
  g8. g16 g4 ges |
  f8. e16 f4 d |
  ees8. d16 d4
  
  %interpellated
  s8 bes'8 |
  a aes g4 ees |
  d8 d f4 d |
  ees8 f g4 fis |
  
  fis8 fis f4 s8 fis |
  fis8 fis fis4 fis |
  g8 g g4 g |
  g8 g g4 g |
  
  a8 g fis8[ f] \break
  
%chorus
  f4 |
  f8. e16 f4 g |
  f8. e16 ees4 ees |
  
  ees8. d16 ees4 f |
  \times 2/3 {d8[ ees] ees} d4 d |
  f8. e16 f4 aes |
  
  g8 g g4 ges |
  f8. e16 f4 d |
  ees8. d16 d4 %\bar"||"
  f4 |
  f8. e16 f4
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
  f,4 |
  bes8. bes16 bes4 bes |
  bes8. bes16 a4 a |
  a8. a16 a4 a |
  bes8 bes bes4 bes |
  bes8. bes16 bes4 bes |
  
  bes8. bes16 bes4 bes |
  bes8. bes16 bes4 bes |
  a8. f16 f4
  %%Interpellated
  
  s4 |
  s4 bes bes |
  bes8 bes bes4 f |
  bes8 bes bes4 bes |
  
  bes8 bes bes4 s8 c8 |
  c c c4 c |
  bes8 bes bes4 bes |
  bes8 bes bes4 bes |
  
  c8 cis c[ a]
  
  %%Chorus
  c4 |
  bes8. bes16 bes4 bes |
  bes8. bes16 a4 a |
  a8. a16 a4 a |
  bes8 bes bes4 bes |
  bes8. bes16 bes4 bes |
  
  bes8 bes bes4 bes |
  bes8. bes16 bes4 bes |
  a8. f16 f4 \bar"||"
  a4 |
  bes8. bes16 bes4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,4 |
  bes,8. bes16 bes4 bes |
  f'8. f16 f4 f |
  
  f8. f16 f4 f |
  bes,8 bes bes4 bes' |
  bes8. bes16 bes,4 d |
  
  ees8. ees16 ees4 ees |
  f8. f16 f4 f |
  f8. bes,16 bes4 
  
  %%Interpolated
  d4\rest |
  d4\rest ees4 ees |
  bes8 bes bes4 f' |
  g8 f ees4 ees |
  
  ees8 ees bes4 d8\rest d |
  d d d4 d |
  <g g,>8 q q4 q |
  q8 q q4 q |
  
  ees8 ees
  d8\fermata[ c\fermata]
  
  %%Chorus
  f4\fermata |
  bes,8. bes16 bes4 bes |
  f'8. f16 f4 f |
  
  f8. f16 f4 f |
  bes,8 bes bes4 bes' |
  bes8. bes16 bes,4 d |
  
  ees8 ees ees4\fermata ees |
  f8. f16 f4 f |
  f8. bes,16 bes4 \bar"||"
  f'4\fermata |
  bes,8. bes16 bes4\fermata \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Sweet Genevieve"}}
  poet = \markup\oldStyleNum"George Cooper (1840–1927)"
  composer = \markup\oldStyleNum"Henry Tucker (1826–1882)"
  tagline = ""
}}


