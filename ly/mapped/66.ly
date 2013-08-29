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
       (padding . 0)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #66
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
}

sopMusic = \relative c' {
  \partial 4
  f8. ees16 |
  d f8. bes c16 bes4 bes8. a16 |
  g bes8. bes g16 f4 f8. ees16 |
  d8. f16 bes8. c16 d8. d16 c8. bes16 |
  c2. \bar"" f,8. ees16 |
  
  d8. f16 bes8. c16 bes4 bes8. a16 |
  g8. bes16 bes8. g16 f4 d'8. c16 |
  bes8. a16 bes8. g16 a8. f16 a8. c16 |
  bes2. bes4\rest \bar"||"
  
  d4 d d8. c16 bes8. g16 |
  f2 bes |
  c4 c d8. c16 bes8. d16 |
  c2. f,8. ees16 |
  
  d8. f16 bes8. c16 bes4 bes8. a16 |
  g8. bes16 bes8. g16 f4 d'8. c16 |
  bes8. a16 bes8. g16 a8. f16 a c8. |
  bes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  In the pris -- on cell I sit, Think -- ing, mo -- ther dear, of you,
  And our bright and hap -- py home so far a -- way,
  And the tears they fill my eyes Spite of all that I can do,
  Tho’ I try to cheer my com -- rades and be gay.
  
  
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  In the bat -- tle front we stood, When their fierc -- est charge they made,
  And they swept us off, a hun -- dred men or more;
  But be -- fore we reached their lines, They were beat -- en back, dis -- mayed,
  And we heard the cry of vic -- t’ry o’er and o’er.
  
  Tramp, tramp, tramp, the boys are march -- ing,
  Cheer up, com -- rades, they will come,
  And be -- neath the star -- ry flag
  We shall breathe the air a -- gain
  Of the free -- land in our own be -- lov -- ed home.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  So with -- in the pris -- on cell We are wait -- ing for the day
  That shall come to o -- pen wide the i -- ron door;
  And the hol -- low eyes grow bright, And the poor heart al -- most gay,
  As we think of see -- ing home and friends once more.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d8. c16 |
  bes d8. d ees16 d4 f8. f16 |
  ees g8. g ees16 d4 d8. c16 |
  bes8. d16 d8. ees16 f8. f16 ees8. d16 |
  f2. c8. c16 |
  
  bes8. d16 d8. ees16 d4 f8. f16 |
  ees8. g16 g8. ees16 d4 f8. ees16 |
  ees8. ees16 ees8. ees16 ees8. ees16 ees8. ees16 |
  d2. s4 |
  
  f4 f f8. d16 ees8. ees16 |
  d4 d d d |
  f4 f f8. ees16 d8. f16 |
  f2. f8. ees16 |
  
  d8. f16 bes8. c16 bes4 bes8. a16 |
  g8. bes16 bes8. g16 f4 f8. ees16 |
  ees8. ees16 ees8. ees16 ees8. ees16 ees ees8. |
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
  f,8. f16 |
  f bes8. f f16 f4 bes8. bes16 |
  bes bes8. bes bes16 bes4 bes8. f16 |
  f8. bes16 bes8. a16 bes8. bes16 a8. bes16 |
  a2. a8. a16 |
  
  bes8. bes16 f8. f16 f4 bes8. bes16 |
  bes8. bes16 bes8. bes16 bes4 f8. f16 |
  g8. g16 g8. g16 c8. a16 c8. a16 |
  bes2. s4 |
  
  bes4 bes bes8. f16 g8. g16 |
  bes4 bes f f |
  a4 a bes bes |
  a4 a a4 f8. ees16 |
  
  d8. f16 bes8. c16 bes4 bes8. a16 |
  g8. bes16 bes8. g16 f4 f8. f16 |
  g8. fis16 g8. g16 c8. a16 c a8. |
  bes2. \bar"|."
}

tenorWords = \lyricmode {
  \repeat unfold 56 ""
  march -- ing on, O cheer up, com -- rades,
  they will come,
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,8. f16 |
  bes, bes8. bes bes16 bes4 d8. d16 |
  ees ees8. ees ees16 bes4 bes8. bes16 |
  bes8. bes16 f'8. f16 f8. f16 f8. f16 |
  f2. f8. f16 |
  
  bes,8. bes16 bes8. bes16 bes4 d8. d16 |
  ees8. ees16 ees8. ees16 bes4 bes8. bes16 |
  ees8. ees16 ees8. ees16 f8. f16 f8. f16 |
  bes,2. d4\rest |
  
  bes4 bes bes8. bes16 bes8. bes16 |
  bes4 bes bes bes |
  f'4 f bes, d |
  f4 f f4 f8. ees16 |
  
  d8. f16 bes8. c16 bes4 bes8. a16 |
  g8. bes16 bes8. g16 f4 bes,8. bes16 |
  ees8. ees16 ees8. ees16 f8. f16 f f8. |
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
    \new Lyrics = "tenors" \with { staff-affinity = #DOWN }
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "tenors" \lyricsto "tenors" \tenorWords
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Tramp! Tramp! Tramp!"}}
  subtitle = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #12.5 \smallCapsOldStyle"The Prisoner’s Hope"}}
  composer = \markup\oldStyleNum"George Frederick Root (1825–1895)"
  tagline = ""
}}
