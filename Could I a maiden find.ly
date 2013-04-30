\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Could I a maiden find"}}
  composer = \markup\oldStyleNum"Folk Song"
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
       (padding . -3)
       (stretchability . 80))
  top-system-spacing =
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  c4\mf |
  f4. g8 a4\< bes |
  c2.\! f,8[ a] |
  c4 c bes\> bes |
  a2\! b4\rest \bar"" f |
  
  a-> c-. c-. d-. |
  c-> bes8[ a] bes4-. c-. |
  a-> a-. a-. c-. ||
  a-> g8[ f] g4-. \bar"" f8[\mf g] |
  
  a2. bes8[\cresc d] |
  c2.\! f,8[\f a] |
  c4 c bes\> bes |
  a2\fermata\! b4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Could I a maid -- en find,
  As good and sweet as kind,
  And fine as silk her nut -- brown hair,
  And dark her eyes, a twink -- ling pair:
  \set associatedVoice = "tenors"
  Then she, \markup\italic then \unset associatedVoice \markup\italic she, then \set associatedVoice = "altos" she, \markup\italic then \unset associatedVoice \markup\italic she,
  then she my love should share.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Her hair is fine and brown,
  She looks de -- mure -- ly down,
  Her eyes are dark, her lips are red,
  She’s all I’ve thought and all I’ve said:
  \set associatedVoice = "tenors"
  And she, \markup\italic and \unset associatedVoice \markup\italic she, and \set associatedVoice = "altos" she, \markup\italic and \unset associatedVoice \markup\italic she,
    and she’s the one I’ll wed.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  And when I came to sue,
  She said she would be true,
  I gave her, bloom -- ing fra -- grant -- ly,
  Of Clove and fair -- est Ros -- ma -- ry.
  \set associatedVoice = "tenors"
  My love, \markup\italic my \unset associatedVoice \markup\italic love, my \set associatedVoice = "altos" love, \markup\italic my \unset associatedVoice \markup\italic love,
  my love is fair to see.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  And she will be my bride,
  And liv -- ing side by side,
  As one we’ll laugh, as one we’ll cry,
  Un -- til we bid the world good -- bye:
  \set associatedVoice = "tenors"
  Then love, \markup\italic then \unset associatedVoice \markup\italic love, then \set associatedVoice = "altos" love, \markup\italic then \unset associatedVoice \markup\italic love,
  then love, good -- bye, bood -- bye!
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \partial 4
  c4 |
  c4. e8 f4 f |
  f2. f4 |
  f f f8[ e] d[ e] |
  f2 s4 f |
  
  f-> a-. a-. bes-. |
  a-> g8[ f] g4-. g-. |
  g-> f8[ e] f4-. f-. |
  f-> e8[ d] e4-. c8[ e] |
  
  f2. f4 |
  f c8[ f] a4 f |
  a f f8[ e] d[ e] |
  f2 s4 \bar"|."
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
  a4 |
  a4. bes8 c4 d |
  a2. a8[ c] |
  c4 a g g |
  f2 s4 a |
  
  c-> c-. c-. c-. |
  c-> c-. c-. c-. |
  c-> c-. d-. c-. |
  c-> c-. g-. a8[ bes] |
  
  c4 a8[ c] f4 bes, |
  a2. c4 |
  c a g c |
  c2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  f,4 |
  f4. f8 f4 f |
  f2. f4 |
  a4 f g c, |
  f2 d4\rest f |
  
  f-> f-. f-. f-. |
  c-> c-. c-. e-. |
  f-> f-. d-. a-. |
  c-> c-. c-. a'8[ g] |
  
  f2. d8[ bes] |
  f'2. a8[ f] |
  c4 c c c |
  f2\fermata d4\rest \bar"|."
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
    \new Lyrics = "altos"
    \new Lyrics = "altosII"
    \new Lyrics = "altosIII"
    \new Lyrics = "altosIV"
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \context Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \context Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \context Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
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


