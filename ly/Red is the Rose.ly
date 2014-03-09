habet \version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Red is the Rose"}}
  composer = \markup\oldStyleNum"Irish Folk Song"
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
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #47
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
  \tieDashed
}

sopMusic = \relative c' {
	\partial 8 |
  c8 |
  f4 f8. g16 a4. f8 |
  g8 a8 g8 f d4. c16[ c] |
  f4 f8. e16 f4 a16 c8. |
  d2 c4. c8 |
  
  d4 d8. c16 a4 a8 c |
  bes16 a8. g f16 d4.\fermata c16[ d] |
  f4 a16 c8. d4 c8 a |
  g2 f2\fermata | \break
  
  
  f4 f8. g16 a4. g16 f |
  g8. a16 g8 f d4. b'8\rest |
  f4 f8. e16 f16 f8. a8 c |
  d2 c2 |
  
  d4 d8. c16 a4 a8 c |
  bes16[ a8.] g8. f16 d4.\fermata c16[ d] |
  f4 a16 c8. d4 c8 a |
  \tieSolid g2 f~ |
  f4 \bar"|."
}
sopWords = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"1. "
	Come o -- ver the hills, my bon -- nie I -- rish lass,
  Come _ o -- ver the hills to your dar -- ling. ""
  You choose the rose, love, and I -- ’ll make the vow,
  And _ I’ll be your true love for -- ev -- er.
  
  \unset ignoreMelismata
  Red is the rose that in yon -- der gar -- den grows;
  Fair is the lil -- y of the val -- ley;
  Clear is the wa -- ter that flows from the Boyne
  But my love is fair -- er than an -- y. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  ’Twas down by Kil -- lar -- ney’s green woods that we strayed
  When the moon and the stars they were shin -- ing.
  The moon shone its rays on her locks of gold -- en hair
  And she swore she’d be my love for -- ev -- er.
}

sopWordsIII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"3. "
  It’s not for the part -- ing that my sis -- ter pains;
  It’s __ _ not for the grief of my moth -- er.
  ’Tis all for the loss of my bon -- nie I -- rish lass
  That _ my heart is break -- ing for -- ev -- er.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 |
  c4 c8. e16 f4. f8 |
  d8 d d8 d bes4. c16[ c] |
  c4 c8. e16 d4 ees16 ees8. |
  d4( f) e4. e8 |
  
  f4 f8. f16 e4 e8 e |
  d16 d8. c c16 bes4. bes16[ d] |
  d4 e16 e8. f4 f8 f |
  e2 f2 |
  
  c4 d8. d16 e4. c16 c |
  d8. d16 bes8 bes bes4. s8 |
  c4 d8. d16 d d8. f8 f |
  f4( e) f2 |
  
  f4 f8. f16 e4 e8 e |
  d4 c8. c16 bes4. c8 |
  c4 f16 f8. f4 f8 f |
  \tieSolid e2 f2~ |
  f4 \bar"|."
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
  a8 |
  a4 a8. bes16 c4. a8 |
  bes8 bes bes8 bes f4. e16[ e] |
  a4 a8. a16 a4 f16 f8. |
  f4( g) g4. a8 |
  
  a4 a8. a16 a4 a8 a |
  g16 g8. f f16 f4. e16[ bes'] |
  a4 a16 a8. bes4 f8 f |
  bes2 a |
  
  a4 a8. a16 a4. a16 a |
  f8. f16 g8 g f4. s8 |
  f4 a8. a16 a a8. c8 a |
  bes2 a |
  
  a4 a8. a16 a4 a8 a |
  g4 f8. f16 f4. g8 |
  f4 c'16 a8. bes4 f8 c' |
  \tieSolid bes2 a~ |
  a4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  f,8 |
  f4 f8. f16 f4. f8 |
  bes,8 bes bes8 bes bes4. c16[ c] |
  f4 f8. f16 d4 c16 c8. |
  bes4( g) c4. a8 |
  
  d4 d8. d16 c4 c8 c |
  g16 g8. a a16 bes4.\fermata c16[ c] |
  d4 c16 c8. bes4 a8 a |
  c2 f\fermata |
  
  f4 d8. d16 c4. a16 a |
  bes8. bes16 g8 g bes4. d8\rest |
  a4 d8. d16 d d8. f8 f |
  bes,4( g) f2 |
  
  d'4 d8. d16 c4 c8 c |
  g4 a8. a16 bes4.\fermata bes8 |
  a4 f'16 f8. bes,4 a8 f |
  \tieSolid c'2 f~ |
  f4 \bar"|."
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
    \tempo 4 = 70
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


