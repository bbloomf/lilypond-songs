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
       (padding . 0.2)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 0))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 0))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 80))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #40
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
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4 ees4 |
  bes'4. c8 bes4 g |
  c4. d8 ees4 c 
  bes4. g8 f4. g8 
  ees2 b'4\rest bes |
  
  ees4. d8 ees4 f |
  ees d c bes |
  c bes ees g, |
  bes2 b4\rest bes4 |
  ees4. d8 ees4 g8[ f] |
  ees4 d c bes |
  
  c bes aes g |
  c2 b4\rest d4 |
  ees4. d8 c4 bes |
  c4 d ees\fermata c |
  bes g f4. g8 |
  ees2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The harp that once through Ta -- ra’s halls,
  The soul of mu -- sic shed,
  Now hangs as mute on Ta -- ra’s walls,
  As if that soul were fled;
  So sleeps the pride of form -- er days,
  So glo -- ry’s thrill is o’er;
  And hearts that once beat high for praise,
  Now feel that pulse no more.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  No more to chiefs and la -- dies bright,
  The harp of Ta -- ra swells;
  The chord, a -- lone, that breaks at night,
  Its tale of ru -- in tells:
  Thus Free -- dom now so sel -- dom wakes,
  The on -- ly throb she gives
  Is when some heart in -- dig -- nant breaks,
  To show that still she lives.
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
  ees4 |
  ees4. ees8 ees4 ees |
  ees4. ees8 ees4 ees |
  ees4. ees8 d4. d8 |
  ees2 s4 g |
  g4. f8 g4 f |
  
  f f aes f |
  g g g ees |
  f2 s4 f |
  g4. f8 g4 aes |
  g f aes f |
  
  ees ees ees ees |
  ees2 s4 aes |
  g4. bes8 aes4 g |
  ees aes g fis |
  g ees d4. d8 |
  bes2. \bar"|."
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
  g4 |
  g4. aes8 g4 bes |
  aes4. bes8 c4 aes |
  g4. bes8 aes4. bes8 |
  g2 s4 g4 |
  c4. c8 c4 c |
  
  bes bes ees bes |
  bes bes c c |
  bes2 s4 bes |
  bes4. bes8 bes4 c |
  bes bes ees bes |
  
  aes g c bes |
  aes2 s4 bes |
  bes4. ees8 ees4 ees |
  c c c ees |
  ees bes aes4. bes8 |
  g2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,4 |
  ees4. ees8 ees4 ees |
  aes,4. aes8 aes4 aes |
  bes4. bes8 bes4. bes8 |
  ees2 d4\rest ees |
  c4. c8 c4 a |
  
  bes bes c d |
  ees ees c c |
  d2 d4\rest d |
  ees4. ees8 ees4 aes, |
  bes bes c d |
  
  ees ees ees ees |
  aes2 d,4\rest f |
  ees4. g8 aes4 ees |
  aes f c\fermata a |
  bes bes bes4. bes8 |
  ees2. \bar"|."
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
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \context Lyrics = "altos"  \lyricsto "tenors" \sopWords
    \context Lyrics = "altosII"  \lyricsto "tenors" \sopWordsII
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The harp that once through Tara’s halls"}}
  poet = \markup\oldStyleNum"Thomas Moore (1779–1852)"
  composer = \markup\oldStyleNum{"Irish Air," \italic"Gramachree"}
  tagline = ""
}}


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
  then love, good -- bye, good -- bye!
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Could I a maiden find"}}
  composer = \markup\oldStyleNum"Folk Song"
  tagline = ""
}}


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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Sole Mio"}}
  poet = \markup\oldStyleNum"Giovanni Capurro (1859–1920)"
  composer = \markup\oldStyleNum"Eduardo di Capua (1865–1917)"
  tagline = ""
}}


