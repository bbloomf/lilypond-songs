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
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 40))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #90
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
  \time 2/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	a'4 a8. a16 |
  d8 c bes a |
  g4 a8 bes |
  a e a4 |
  a a8. a16 |
  d8 c bes g |
  
  b\rest f g a |
  f8. e16 d4 |
  a'8. a16 bes8 g |
  f e d' c |
  f,8. f16 e8 f |
  g gis a4 \bar"||"\break
  
  a8. e16 e8 e |
  gis8 b cis a |
  a8. e16 e8 e |
  gis b a4 |
  a8-^ e a-^ e |
  
  a4 a,\fermata |
  a'8 d, a' d, |
  g a16[ bes] a8 d\fermata |
  a16^\markup\italic"rall." bes a g f8[ e] |
  d4\fermata b'4\rest \bar":|"\break
  
  \key d\major
  fis4^\markup{\dynamic"p" \italic"More slowly, with feeling"} g8 a |
  a e fis g |
  fis4 cis'8 b |
  a dis, e4 |
  fis a8 a |
  d d, e eis |
  
  fis fis \acciaccatura {fis16[ a]} g8. fis16 |
  fis8 e d4 |
  fis8.^\markup\italic"poco rall." fis16 gis8 a |
  b b a gis |
  a4 gis8 fis |
  cis' gis gis4 \bar "||"
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	He was a Pun -- chin -- el -- lo,
  Sweet Col -- um -- bine was she,
  He loved the ground she danced on,
  She laughed his love to see,
  Till he laughed him -- self as gai -- ly,
  Danc -- ing, jok -- ing ev -- ’ry night;
  
  \set stanza = #"1, 3. "
  “He’s the mad -- dest, mer -- riest fel -- low!”
  Cried the peo -- ple with de -- light.
  “Bra -- vo! Bra -- vo! Bra -- vo! Bra -- vo! Bra -- vo!
  Pun -- chin -- el -- lo! Bra -- vo, Pun -- chin -- el -- lo!”
  
  \set stanza = #"3. "
  One win -- ter morn they told him
  Sweet Col -- um -- bine was dead;
  He nev -- er joked so gai -- ly
  As that night, the peo -- ple said,
  Nev -- er sang and laughed so mad -- ly,
  Ah! for his heart that night!
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Bright was the day she mar -- ried,
  And there a -- mong the rest,
  Came poor old Pun -- chin -- el -- lo,
  He was the blith -- est guest,
  Had they seen his tears at mid -- night,
  In his gar -- ret near the sky,
  
  \set stanza = #"2, 4. "
  “He’s the mad -- dest, quaint -- est fel -- low!”
  That would still have been their cry.
  %“Bra -- vo! Bra -- vo! Bra -- vo! Bra -- vo! Bra -- vo!
  %Pun -- chin -- el -- lo! Bra -- vo, Pun -- chin -- el -- lo!”
}

sopWordsIII = \lyricmode {
  \set stanza = #"4. "
  But when the play was o -- ver,
  Forth to her grave he crept,
  Laid one white rose up -- on it,
  Then sat him down and wept;
  But the peo -- ple, had they seen him
  Gaz -- ing to the moon -- lit sky,
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  d4 e8. e16 |
  fis8 fis fis fis |
  d4 d8 d |
  e e e4 |
  fis4 e8. e16 |
  a8 a g d |
  
  s d e f |
  d8. cis16 d4 |
  f8. f16 e8 e |
  f e e e |
  c8. c16 b8 b |
  e e f4 \bar"||"
  
  cis8. cis16 cis8 cis |
  e e e e |
  e8. cis16 cis8 cis |
  e e e4 |
  a8 e a e |
  
  a4 a, |
  d8 d d d |
  d f16[ g] f8 f |
  f16 g f e d8[ cis] |
  d4 s \bar":|"
  
  \key d\major
  d4 d8 d |
  cis cis d cis |
  d4 f8 f |
  e bis cis4 |
  d e8 e |
  fis d d d |
  
  d d d8. d16 |
  cis8 cis d4 |
  cis8. cis16 eis8 fis |
  d d eis eis |
  fis4 eis8 fis |
  eis8 eis eis4 \bar"||"
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
  f,4 a8. a16 |
  a8 a d d |
  d4 d8 d |
  cis8 cis cis4 |
  d cis8. cis16 |
  d8 d d bes |
  
  s a a a |
  a8. g16 f4 |
  c'8. c16 c8 c |
  c c g g |
  aes8. aes16 g8 g |
  bes c c4 \bar"||"
  
  a8. a16 a8 a |
  b gis a cis |
  cis8. a16 a8 a |
  b d cis4 |
  a8 e a e |
  
  a4 a, |
  f'8 f f f |
  bes d d a |
  a16 a a a a8[ g] |
  f4 s \bar":|"
  
  \key d\major
  a4 a8 a |
  a a a a |
  a4 gis8 gis |
  a a a4 |
  a a8 a |
  b b b b |
  
  d d d8. d16 |
  a8 g fis4 |
  a8. a16 cis8 cis |
  b b b cis |
  cis4 b8 a |
  gis cis cis4 \bar"||"
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,4^\markup\italic{ \halign #-0.625 "Sing 4th verse slowly and with feeling."} cis8. cis16 |
  d8 d d d |
  bes'4 a8 g |
  a a a4 |
  d, e8. e16 |
  fis8 fis g g |
  
  d\rest a a a |
  a8. a16 d4 |
  f8. f16 g8 bes |
  a g c, c |
  d8. d16 d8 d |
  c c f4 \bar"||"
  
  a,8. a16 a8 a |
  e' e e e |
  e8. e16 e8 e |
  e e a4 |
  a8 e a e |
  
  a4 a,\fermata |
  d8 d d d |
  d d d d\fermata |
  a16 a a a a4 |
  d4\fermata d\rest \bar":|"
  
  \key d\major
  d4 e8 fis |
  g g fis e |
  d4 d8 d |
  cis a a4 |
  d4 cis8 cis |
  b b g' gis |
  
  a a b8. a16 |
  a,8 a d4 |
  fis8. fis16 fis8 fis |
  gis8 gis cis, cis |
  cis4 cis8 cis |
  cis cis cis4 \bar"||"
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Punchinello"}}
  composer = \markup\oldStyleNum"James Lynam Molloy (1837–1909)"
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  tagline = ""
}}


global = {
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \transpose d ees {
  \relative c' {
    \partial 8 
    d8 |
    d g fis g
    a4 a8\rest a |
    b a fis d
    d4 a'8\rest b |
    b4 ais8 b
    
    d4 c8 a |
    g~ g g8 b
    a4 a8\rest d, |
    d g fis g
    a4. a8 |
    
    b8 a fis d
    b'4. \teeny b8 \normalsize |
    b c e, a
    g4 fis |
    g2. a4\rest \bar"||"
    d4 a
    a a8 b |
    
    g2
    d4 a'\rest |
    d a8 a
    a4 g8 a |
    b2. a8\rest d,8 |
    d g fis g
    
    a4. a8 |
    b a fis d
    b'4. b8 |
    b c e, a
    g4 fis |
    g2. a8\rest \bar"|."
  }
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	The morn of life is past,
  And eve -- ning comes at last,
  It brings me a dream of a once __ hap -- py day,
  Of mer -- ry forms I’ve seen
  Up -- on the vil -- lage green, ""
  Sport -- ing with my old dog Tray.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  The forms I call’d my own
  Have van -- ish’d one by one,
  The loved ones, the dear ones have all __ pass’d a -- way,
  Their hap -- py smiles are flown,
  Their gen -- tle voic -- es gone,
  I’ve no -- thing left but old dog Tray.
  
  Old dog Tray’s ev -- er faith -- ful,
  Grief can -- not drive him a -- way;
  He’s gen -- tle, he is kind,
  I’ll nev -- er, nev -- er find
  A bet -- ter friend than old dog Tray.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  \set ignoreMelismata = ##t
  When thoughts re -- call the past,
  His eyes are on me cast,
  I know that he feels what my break -- ing heart would say;
  Al -- though he can -- not speak,
  I’ll vain -- ly, vain -- ly seek
  A bet -- ter friend than old dog Tray.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  c8 |
  c ees ees ees g4 s8 g |
  g g ees ees c4 s8 ees |
  ees4 ees8 ees f4 f8 f |
  
  ees~ ees ees8 ees ees4 s8 des |
  c ees ees ees g4 s8 g |
  g g ees ees ees4. \teeny ges8 \normalsize |
  
  ges f f f ees4 ees |
  ees2. s4 \bar"||"
  g4 g g g8 g |
  ees2 ees4 s |
  g g8 g g4 f8 g |
  
  aes2. s8 c, |
  c ees ees ees g4. g8 |
  g g ees ees ees4. ges8 |
  ges f f f ees4 ees |
  ees2. s8 \bar"|."
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
  aes8 |
  aes aes bes c ees4 s8 ees |
  ees des bes g aes4 s8 aes |
  aes4 aes8 aes aes4 bes8 des |
  
  c~ c aes8 aes g4 s8 g |
  aes aes bes c ees4 s8 ees |
  ees des bes g aes4. \teeny aes8 \normalsize |
  
  aes8 bes des des c4 des |
  c2. s4 \bar"||"
  bes4 ees ees ees8 ees |
  c2 c4 s |
  bes ees8 ees ees4 c8 ees |
  
  ees2. s8 aes,8 |
  aes aes bes c ees4. ees8 |
  ees des bes g aes4. aes8 |
  aes bes des des c4 des |
  c2. s8 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  aes,8 |
  aes c ees ees ees4 d8\rest ees |
  ees ees ees ees aes,4 d8\rest aes'|
  aes4 aes8 aes des,4 bes8 bes |
  
  ees~ ees c8 aes ees'4 d8\rest ees |
  aes, c ees ees ees4 d8\rest ees |
  ees ees ees ees aes,4. \teeny aes8 \normalsize |
  
  aes des des bes ees4 ees |
  aes2. d,4\rest \bar"||"
  ees4 ees ees ees8 ees |
  aes2 aes4 d,\rest |
  ees ees8 ees ees4 ees8 ees |
  
  aes2. d,8\rest aes'|
  aes c, ees ees ees4. ees8 |
  ees ees ees ees aes,4. aes8 |
  aes des des bes ees4 ees |
  aes2. d,8\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Old Dog Tray"}}
  composer = \markup\oldStyleNum"Stephen Foster (1826–1864)"
  tagline = ""
}}


