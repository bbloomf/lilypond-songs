\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premr Pro Smdb Subh"){ \abs-fontsize #18 \smallCapsOldStyle"Listen to the Mocking Bird"}}
  composer = \markup\oldStyleNum"Septimus Winner (1827–1902) and Richard Milburn"
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
       (padding . -1)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #86
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premr Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premr Pro" "Garamond Premr Pro" "Garamond Premr Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \slurDashed
}

sopMusic = \relative c' {
	\partial 4
  d4 |
  d4. d8 fis4 e8.( fis16) |
  a8 g4.~ g8 b\rest e,8.( d16) |
  d8 fis4.~ fis8 b8\rest e,8.( d16) |
  d8 g4.~ g8 b\rest d,4 |
  d4. d8 fis4 e8.( fis16) |
  
  a8 g4.~ g8 b\rest b b |
  c e e e e d c a |
  g2 b4\rest \bar"" d,4 |
  d4. d8 fis4 e8. fis16 |
  
  a8 g4.~ g8 b\rest e,8. d16 |
  d8 fis4.~ fis8 b8\rest e,8. d16 |
  d8 g4.~ g8 b\rest d,4 |
  d4. d8 fis4 e8. fis16 |
  a8 g4.~ g8 b\rest b b |
  
  c e e e e d c a |
  g2.
  \repeat volta 2 {
    b16 b b b |
    b8 a a4 b\rest a16 a a a |
    
    a8 g g4 b\rest g |
    a8 a a a a c b a |
    b2 b4\rest b16 b b b |
    
    b8 a a4 b\rest a16 a a a |
    a8 g g4 b\rest b |
    c8 e e e e d c a |
    g2 b4\rest
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	\set ignoreMelismata = ##t
  I’m dream -- ing now of sweet Hal -- lie, __ _
  my sweet Hal -- lie, __ _ my sweet Hal -- lie, __ _
  I’m dream -- ing now of my Hal -- lie, __ _
  For the thought of her is one that nev -- er dies;
  
  \unset ignoreMelismata
  She’s sleep -- ing here in the val -- ley, __
  in the val -- ley, __ in the val -- ley, __
  She’s sleep -- ing here in the val -- ley, __
  And the mock -- ing bird is sing -- ing where she lies.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
	\set ignoreMelismata = ##t
  Ah! well I can yet re -- mem -- ber, __ _
  I re -- mem -- ber, __ _ I re -- mem -- ber, __ _
  Ah! well I can yet re -- mem -- ber, __ _
  When we gath -- ered in the cot -- ton side by side;
  
  \unset ignoreMelismata
  ’Twas in the mild \once \override LyricHyphen #'minimum-distance = #0.7 mid -- Sep -- tem -- ber, __
  in Sep -- tem -- ber, __ in Sep -- tem -- ber, __
  ’Twas in the mild \once \override LyricHyphen #'minimum-distance = #0.7 mid -- Sep -- tem -- ber,
  And the mock -- ing bird was sing -- ing far and wide.
  
  Lis -- ten to the mock -- ing bird,
  Lis -- ten to the mock -- ing bird,
  The mock -- ing bird still sing -- ing o’er her grave;
  Lis -- ten to the mock -- ing bird,
  Lis -- ten to the mock -- ing bird,
  Still sing -- ing where the weep -- ing wil -- lows wave.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  When charms of spring a -- wak -- en, __
  a -- wak -- en, __ a -- wak -- en, __
  When charms of spring a -- wak -- en, __
  And the mock -- ing bird is sing -- ing on the bough,
  
  I feel like one so for -- sak -- en, __
  so for -- sak -- en, __ so for -- sak -- en, __
  I feel like one so for -- sak -- en, __
  Since my Hal -- lie is no long -- er with me now.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 |
  d4. d8 d4 e8.( d16) |
  d8 d4.~ d8 s b8.( d16) |
  d8 d4.~ d8 s c8.( d16) |
  d8 d4.~ d8 s \bar"" d4 |
  d4. d8 d4 e8.( d16) |
  
  d8 d4.~ d8 s g g |
  g g g g fis fis fis fis |
  d2 s4 d4 |
  d4. d8 d4 e8. d16 |
  
  d8 d4.~ d8 s b8. d16 |
  d8 d4.~ d8 s c8. d16 |
  d8 d4.~ d8 s d4 |
  d4. d8 d4 e8. d16 |
  d8 d4.~ d8 s g g |
  
  g g g g fis fis fis fis |
  d2.
  
  \repeat volta 2 {
    d16 d d d |
    d8 c c4 s c16 c c c |
    
    c8 b b4 s b |
    d8 d d d d fis g d |
    g2 s4 d16 d d d |
    
    d8 c c4 s c16 c c c |
    c8 b b4 s f' |
    e8 g g g fis fis fis fis |
    d2 s4
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
  b4 |
  \tieDashed b4. b8 c4 c8.~ c16 |
  \tieSolid c8 b4.~ b8 s \tieDashed b8.~ b16 |
  \tieSolid c8 c4.~ c8 s \tieDashed a8.~ a16 |
  \tieSolid b8 b4.~ b8 s b4 |
  c4. c8 a4 \tieDashed c8.~ c16 |
  
  \tieSolid
  c8 b4.~ b8 s d d |
  e c c c a a a c |
  b2 s4 b4 |
  b4. b8 c4 c8. c16 |
  
  c8 b4.~ b8 s b8. b16 |
  c8 c4.~ c8 s a8. a16 |
  b8 b4.~ b8 s b4 |
  c4. c8 a4 c8. c16 |
  c8 b4.~ b8 s d d |
  
  e c c c a a a c |
  b2.
  
  \repeat volta 2 {
    g16 g g g |
    g8 fis fis4 s fis16 fis fis fis |
    
    g8 g g4 s b |
    c8 c c c c a g fis |
    g2 s4 g16 g g g |
    
    g8 fis fis4 s fis16 fis fis fis |
    g8 g g4 s d' |
    c8 c c c c c c a |
    b2 s4
  }
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4 |
  \tieDashed g4. g8 d4 d8.~ d16 |
  \tieSolid g8 g4.~ g8 d\rest \tieDashed g8.~ g16 |
  \tieSolid d8 d4.~ d8 d\rest \tieDashed d8.~ d16 |
  \tieSolid g8 g4.~ g8 d\rest g4 |
  d4. d8 d4 \tieDashed d8.~ d16 |
  
  \tieSolid
  g8 g4.~ g8 d\rest g g |
  c, c c c d d d d |
  g2 d4\rest g4 |
  g4. g8 d4 d8. d16 |
  
  g8 g4.~ g8 d\rest g8. g16 |
  d8 d4.~ d8 d\rest d8. d16 |
  g8 g4.~ g8 d\rest g4 |
  d4. d8 d4 d8. d16 |
  g8 g4.~ g8 d\rest g g |
  
  c, c c c d d d d |
  g2.
  
  \repeat volta 2 {
    g16 g g g |
    d8 d d4 d\rest d16 d d d |
    
    g8 g, g4 d'\rest g |
    fis8 fis fis fis fis d d d |
    g,2 d'4\rest g16 g g g |
    
    d8 d d4 d\rest d16 d d d |
    g8 g, g4 d'\rest g, |
    c8 c c c d d d d g,2 d'4\rest 
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


