\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"My Old Kentucky Home"}}
  composer = \markup\oldStyleNum"Stephen Foster (1826–1864)"
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
  first-page-number = #89
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDotted\slurDotted
}

sopMusic = \relative c' {
	\repeat volta 2 {
    \partial 4
    b'8\rest b |
    b4 b g a8. b16 |
    c8. b16 c8 e d4 \bar"" \teeny d8 \normalsize c |
    b a b\rest g g fis b\rest g |
    a2. \bar""
    
    \teeny a8 \normalsize a |
    b4 b g a8. b16 |
    c8 b c8. e16 d4 \bar"" g,8. a16 
  }
  \alternative {
    {
      b4  b8~ b a( g) b8.( a16) |
      g2.
    }
    
    {
      \partial 1
      b8 g c b a4. fis8 |
      g2. b4\rest
    }
  }
  \repeat volta 2 {
    d4. b8 c4. e8 |
    d b4. b4\rest a |
    g4. a8 g4. e8 |
    g2 b4\rest \bar""
    
    g8 a |
    b4 b g a8 b |
    c8. b16 c8 e d4\fermata g,8. a16 |
    b8. g16 c8 b a4 a8. fis16 |
    g2.
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
  \set ignoreMelismata = ##t
	The sun shines bright in the old Ken -- tuck -- y home, ""
  ’Tis sum -- mer, the dark -- ies are gay:
  "" The corn -- top’s ripe and the mead -- ow’s in the bloom,
  While the birds make _ mu -- sic all the day.
  
}
sopWordsA = \lyricmode {
  The young folks roll on the lit -- tle cab -- in floor, ""
  All mer -- ry, all hap -- py and bright;
  By -- ’n -- by hard times comes a -- knock -- ing at the door,
  Then my
}

sopWordsII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"2. "
  They hunt no more for the pos -- sum and the coon,
  On the mead -- ow, the hill, and the shore;
  "" They sing no more by the glim -- mer of the moon,
  On the bench by the old _ cab -- in door.
  
  old Ken -- tuck -- y home, good -- night!
  
  Weep no more, my la -- dy,
  O weep no more to -- day!
  We will sing one song for the old Ken -- tuck -- y home,
  For the old Ken -- tuck -- y home, far a -- way.
}
sopWordsIIa = \lyricmode {
  The day goes by like a shad -- ow o’er the heart, ""
  With sor -- row where all was de -- light;
  "" The time has come when the dark -- ies have to part,
  Then my
}

sopWordsIII = \lyricmode {
  \set ignoreMelismata = ##t
  \set stanza = #"3. "
  The head must bow and the back will have to bend, ""
  Wher -- ev -- er the dark -- ey may go;
  "" A few more days, and the trou -- ble all will end,
  In the field where the su -- gar -- canes _ grow;
}
sopWordsIIIa = \lyricmode {
  A few more days for to tote the wea -- ry load— ""
  No mat -- ter, ’twill nev -- er be light;
  "" A few more days till we tot -- ter on the road,
  Then my
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  \repeat volta 2 {
    s8 g' |
    g4 g d fis8. g16 |
    g8. g16 g8 g g4 \teeny g8 \normalsize g |
    g d s d cis cis s cis |
    d2. \bar""
    
    \teeny fis8 \normalsize fis |
    g4 g d fis8. g16 |
    g8 g g8. g16 g4 g8. g16
  }
  \alternative {
    {
      g4 g8~ g fis( g) fis8.~ fis16 |
      g2.
    }
    
    {
      \partial 1
      g8 g g g fis4. d8 |
      g2. s4 |
    }
  }
  \repeat volta 2{
    g4. g8 g4. g8 |
    g g4. s4 fis |
    e4. e8 e4. c8 |
    d2 s4
    
    g8 g |
    g4 g g g8 g |
    g8. g16 g8 g g4 d8. d16 |
    g8. d16 g8 g fis4 fis8. d16 |
    d2.
  }
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
  \repeat volta 2{
    s8 d |
    d4 d b c8. d16 |
    e8. d16 e8 c b4 \teeny b8 \normalsize e |
    d c s b a a s g |
    fis2. \bar""
    
    \teeny d'8 \normalsize d |
    d4 d b c8. d16 |
    e8 d e8. c16 b4 b8. cis16 |
  }
  \alternative {
    {
      d4 d8~ d c( b) d8.( c16) |
      b2.
    }
    
    {
      \partial 1
      d8 b e d c4. c8 |
      b2. s4 |
    }
  }
  \repeat volta 2{
    b4. d8 e4. c8 |
    b d4. s4 c4 |
    b4. b8 c4. g8 |
    b2 s4
    
    b8 c |
    d4 d b c8 d |
    e8. d16 e8 c b4 b8. c16 |
    d8. b16 e8 d d4 c8. c16 |
    b2.
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat volta 2{
    d,8\rest g |
    g4 g g g8. g16 |
    g8. g16 g8 g g4 \teeny g8 \normalsize g |
    g g d\rest g e e d\rest e |
    d2. \bar""
    
    \teeny d8 \normalsize d |
    g4 g g g8. g16 |
    g8 g g8. g16 g4 e8. e16 |
  }
  \alternative {
    {
      d4 d8~ d d~ d d8.~ d16 |
      g2.
    }
    
    {
      \partial 1
      d8 d d d d4. d8 |
      g2. d4\rest |
    }
  }
  \repeat volta 2 {
    g4. g8 g4. g8 |
    g g4. d4\rest d |
    e4. e8 c4. c8 |
    g2 d'4\rest \bar""
    
    g8 g |
    g4 g g g8 g |
    g8. g16 g8 g g4\fermata g8. g16 |
    d8. d16 d8 d d4 d8. d16 |
    g2.
  }
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
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWordsA
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsIIa
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIIIa
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
}

\score {
  \unfoldRepeats
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWordsA
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsIIa
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIIIa
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
    \tempo 4 = 85
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
}

