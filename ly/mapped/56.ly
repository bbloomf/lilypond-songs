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
  first-page-number = #56
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
  \tempo 4 = 126
  \key g \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
  \tieDashed
}

sopMusic = \relative c' {
	\repeat volta 2 {
    \partial 4
    e8 b'\rest |
    d, b'\rest e, b'\rest fis b\rest e,[ d] |
    g b\rest a b\rest b b\rest \teeny a g \normalsize |
    d4 e8[ e] fis4 e8[ d] |
    g2 b4\rest \bar"" g |
    
    e e8 fis g4 fis8[ e]
    a8 b\rest b b\rest c b\rest b[ a] |
    << {b~ b4.} {s4. \teeny b8} >> \normalsize b4 b8.~ b16 |
    e,2 b'4\rest \bar"" e,4 |
    
    d8 b'\rest e, b'\rest fis b\rest e,[ d] |
    g b\rest a b\rest b b\rest  a[ g] |
    \slurDashed
    a4 a a( g8) e |
    \slurSolid
    d2 b'4\rest \bar"" d,8[ d] |
    
    \tempo 4 = 92
    c'4. c8 b4. b8 |
    a4 e g fis8[ e] |
    b'4. b8 fis4~ fis8 g |
    e2.\fermata \bar"" e4 |
    d2 a'4 d, |
    
    d2 b'4 d, |
    c'4 c8 c c4 c ||
    c2( b4) \bar""\break << b4\rest {s8 \teeny b} >> | \normalsize
    b4 b8 b gis4 b8\rest gis |
    a4 b8[ b] c4 g8[ e] |
    d4. d8 d4. d8 |
    g2.
  } \break
  
  b4 |
  b4 b8\rest b b4. b8 |
  c4. b8 b4 b8\rest b |
  b4. a8 g4 a |
  fis2. \bar""\break b8\rest b |
  
  b4. b8 b4 b8\rest b |
  e4 d c b8\rest c |
  c4 cis d4. a8 |
  b2 b4\rest b |
  a4. a8 g4 \bar"" b8\rest g |
  
  c4 c b b8\rest b |
  e4 e a, g |
  d'2 b4\rest d, |
  c'8 c4. b4. b8 |
  a4 e g\fermata \bar""\break b8\rest a |
  
  b4 d, \acciaccatura b'8 a4. g8 |
  g2.\fermata \bar"" d4 |
  d2 a'4 d, |
  d2 b'4 d, |
  c' c c c8 c |
  
  c2( b4) \bar"" b8\rest b |
  b4 b8 b gis4 b8\rest gis |
  a4 b c4\fermata g8[ e] |
  d4 b'8\rest d,8 d4 b'8\rest d, |
  \tieSolid
  d1~ |
  d2 b'4\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Me -- thought the stars were blink -- ing bright,
  And the old brig’s sails un -- furled;
  \set ignoreMelismata = ##t
  I said, “I will sail to my love this night
  At the oth -- er "" side of the world.”
  I stepp’d a -- board, we _ sail’d so fast,
  The _ sun shot up from the bourn;
  But a dove that perch’d up -- on the mast
  Did _ mourn, and mourn, _ and mourn.
  
  O fair dove! O fond dove!
  And dove with the white, white breast, __ _ ""
  Let me a -- lone, the dream is my own,
  And my heart is full of rest.
  
  \set stanza = #"3. "
  \unset ignoreMelismata
  My love! He stood at my right hand,
  His eyes were grave and sweet.
  Me -- thought he said, “In this far land,
  O, is it thus we meet?
  Ah! maid, most dear, I am not here;
  I have no place, no part,
  No dwell -- ing more by sea or shore,
  But on -- ly in thy heart.”
  
  O fair dove! O fond dove!
  Till night rose o -- ver the bourn
  The dove on the mast, as we sail’d fast,
  Did mourn, and mourn, and mourn. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  My true love fares on _ this great hill, "" ""
  Feed -- ing his sheep for _ aye;
  I look’d in his hut, but _ all was still,
  My _ love _ was gone a -- _ way.
  I went to gaze in the for -- est creek,
  And the dove mourn’d on __ _ a -- pace;
  No _ flame did flash, nor fair blue reek
  Rose _ up to show me his place.
  
  O last love! O first love!
  My love with the true, true heart, __ _
  To think I have come to this thy __ _ home,
  And _ yet we are a -- part.
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
  \repeat volta 2 {
    b8 s |
    c s c s c s c4 |
    b8 s d s d s \teeny d d | \normalsize
    c4 c8[ c] c4 c |
    b2 s4 d |
    
    e e8 dis e4 dis8[ e] |
    e8 s e s e s e[ e] |
    << {e~ e4.} {s4. \teeny e8} >> \normalsize dis4 dis8.~ dis16 |
    e2 s4 e |
    
    d8 s e s c s c[ c] |
    b s d s d s d[ d] |
    fis4 fis e~ e8 cis |
    d2 s4 d8[ d] |
    
    e4. e8 e4. e8 |
    e4 e e cis |
    e4. e8 dis4~ dis8 dis |
    e2. b4 |
    c2 c4 c |
    
    b2 d4 d |
    fis g8 g a4 a |
    g2. s8 \teeny g8 \normalsize |
    e4 e8 e e4 s8 e |
    e4 e8[ e] e4 e8[ e] |
    
    d4. b8 c4. c8 |
    b2.
  }
  
  e4 |
  e4 s8 e e4. e8 |
  dis4. dis8 dis4 s8 dis |
  e4. e8 e4 e |
  dis2. s8 dis! |
  
  e4. e8 e4 s8 e8 |
  gis4 b a s8 a |
  a4 a fis4. fis8 |
  g2 s4 d |
  d4. d8 d4 s8 d |
  
  e4 d d s8 g |
  g4 g e e |
  fis2 s4 d |
  e8 e4. e4. e8 |
  e4 e e s8 e |
  
  d4 b c4. b8 |
  b2. b4 |
  c2 c4 c |
  b2 d4 d |
  fis g a a8 a |
  
  g2. s8 g |
  e4 e8 e e4 s8 e |
  e4 e e e |
  d s8 b c4 s8 c |
  \tieSolid
  b1~ b2 s4 \bar"|."
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
  \repeat volta 2 {
    \partial 4
    b8 s |
    fis s g s a s g[ fis] |
    g s fis s g s \teeny b b | \normalsize
    fis4 g8[ g] a4 g8[ fis] |
    g2 s4 b |
    
    g g8 a b4 a8[ g] |
    a s gis s a s gis[ a] |
    << { g?~ g4. } {s4. \teeny g8} >> \normalsize fis4 fis8.( a16) |
    g2 s4 g |
    
    a8 s g s a s g[ fis] |
    g s fis s g s b[ b] |
    d4 d cis~ cis8 g |
    fis2 s4 fis8[ fis] |
    
    a4. a8 gis4. gis8 |
    a4 a bes g |
    g4. g8 fis4~ fis8 b |
    g2. g4 |
    a2 fis4 fis |
    
    g2 g4 b |
    a c8 c e4 fis |
    e2( d4) s8 \teeny d \normalsize |
    gis,4 gis8 gis b4 s8 b |
    a4 gis8[ gis] a4 bes8[ bes] |
    
    b?4. g8 fis4. fis8 |
    g2.
  }
  
  g4 |
  g4 s8 g g4. g8 |
  a4. fis8 fis4 s8 fis |
  g4. c8 b4 a |
  b2. s8 fis |
  
  g4. g8 g4 s8 g |
  b4 e e s8 e |
  d4 d c4. c8 |
  d2 s4 g, |
  fis4. fis8 g4 s8 g |
  
  g4 fis g s8 g |
  c4 c a a |
  a2 s4 fis |
  a8 a4. gis gis8 |
  a4 a bes s8 a |
  
  g4 g fis4. g8 |
  g2. g4 |
  fis2 fis4 fis |
  g2 g4 b |
  a c e fis8 fis |
  
  e2( d4) s8 d |
  gis,4 gis8 gis b4 s8 b |
  a4 gis a bes |
  b? s8 g a4 s8 fis |
  \tieSolid
  g1~ |
  g2 s4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \repeat volta 2 {
    \partial 4
    g8 d\rest |
    d d\rest d d\rest d d\rest d4 |
    g,8 d'\rest d d\rest g d\rest \teeny g g | \normalsize
    d4 d8[ d] d4 d |
    g,2 d'4\rest g |
    
    e e8 e e4 e8[ e] |
    c8 d\rest b d\rest a d\rest b[ c] |
    << { b~ b4. } {s4. \teeny b8} >> \normalsize b4 b8.~ b16 |
    e2 d4\rest e |
    
    fis8 d\rest e d\rest d d\rest d[ d] |
    g,8 d'\rest d d\rest g d\rest g[ g] |
    a4 a a~ a8 a, |
    d2 d4\rest d8[ d] |
    
    a4. a8 b4. b8 |
    c4 c cis bes |
    b?4. b8 b4~ b8 b |
    e2.\fermata e4 |
    fis2 d4 d |
    
    g2 g,4 g' |
    d e8 e d4 d |
    g2. << d4\rest {s8 \teeny g} >> | \normalsize
    e4 e8 e e4 d8\rest d |
    c4 b8[ b] a4 cis8[ cis] |
    
    d4. d8 d4. d8 |
    g,2.
  }
  
  e'4 |
  e4 d8\rest e e4. e8 |
  fis4. b,8 b4 d8\rest b |
  e4. e8 e4 c |
  b2. d8\rest b |
  
  e4. e8 e4 d8\rest e |
  e4 gis a d,8\rest g |
  fis4 fis d4. d8 |
  g2 d4\rest g, |
  c4. c8 b4 d8\rest b |
  
  a4 d g d8\rest g |
  c,4 c cis cis |
  d2 d4\rest d |
  a8 a4. b b8 |
  c4 c cis\fermata d8\rest cis |
  
  d4 d d4. d8 |
  g,2. g4 |
  d'2 d4 d |
  g,2 g'4 g |
  d e d d8 d |
  
  g2. d8\rest g |
  e4 e8 e e4 d8\rest d |
  c4 b a cis |
  d d8\rest d d4 d8\rest d |
  \tieSolid
  g,1~ |
  g2 d'4\rest \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"O Fair Dove, O Fond Dove"}}
  composer = \markup\oldStyleNum"Alfred Scott Gatty (1847–1918)"
  poet = \markup\oldStyleNum"Jean Ingelow (1820–1897)"
  tagline = ""
}}


