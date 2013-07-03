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
       (stretchability . 150))
  score-markup-spacing =
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
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	g'8[ g] g bes4 bes8 |
  << {g ees4~ ees4.} {s2 s8 \teeny ees} >> | \normalsize
  c'\< c c d4\> c8 |
  << {bes4.~\! bes} {s2 s8 \teeny bes8} >> | \normalsize \break
  
  \tieDashed
  bes4( bes8) c8[\< c] c |
  \tieSolid
  d\! << {bes4~ bes4.} {s2 \teeny f8} >> | \normalsize
  bes8[ bes] bes c4 c8\< |
  d4.~ d4.\fermata\! |\break
  
  ees4~->\f ees8 d4 c8\> |
  bes8-> ees,4~ ees8\! ees--[ f--] |
  g bes aes g4 f8 |
  bes4.~ bes | \break
  
  ees4.-> d4 c8 |
  bes-> ees,4~ ees8 ees--[ f--] |
  g^\markup\italic"poco rit." bes aes g4 f8 |
  ees4.~ ees \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Jol -- ly boat -- ing weath -- er __ ""
  And a hay har -- vest breeze; __ ""
  Oars on the feath -- er, __ ""
  Glid -- ing by the trees; __
  Swing, swing to -- geth -- er, __
  \set ignoreMelismata = ##t
  With your bod -- ies be -- tween your knees; __ _
  Swing, swing to -- geth -- er, __ _
  With your bod -- ies be -- tween your knees. __ _
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  \set ignoreMelismata = ##t
  Twen -- ty years hence such weath -- er __ _
  May call us from of -- fice stools; __ _
  We may be slow on the feath -- er __ _
  And called by the boys, old fools; __ _
  Still we’ll swing to -- geth -- er, __ _
  And __ _ swear by the best of schools; _
  Swing, swing to geth -- er, __ _
  And __ _ swear by the best of schools. _
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
  ees8[ ees] ees d4 d8 |
  << {bes bes4~ bes4.} {s2 s8 \teeny bes} >> | \normalsize
  ees8 ees ees f4 f8 |
  << {g4.~ g} {s2 s8 \teeny g}>> | \normalsize
  
  f4~ f8 f[ f] f |
  << {f d4~ d4.} {s2 s8 \teeny d} >> \normalsize
  d8[ d] f f4 f8 |
  f4.( aes) |
  
  g4~ g8 aes4 aes8 |
  g ees4~ ees8 ees[ ees] |
  ees ees f ees4 d8 |
  ees4.~ ees |
  
  ees4. aes4 aes8 |
  g ees4~ ees8 ees[ ees] |
  ees ees f ees4 d8 |
  bes4.~ bes \bar"|."
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
  bes8[ bes] bes aes4 aes8 |
  << {g g4~ g4.} {s2 s8 \teeny g}>> | \normalsize
  aes8 aes aes bes4 d8 |
  << {ees4.~ ees} {s2 s8 \teeny ees}>> | \normalsize
  
  d4~ d8 a[ a] a |
  << {bes bes4~ bes4.} {s2 s8 \teeny bes}>> | \normalsize
  bes[ bes] bes a4 a8 |
  bes4.~ bes |
  
  bes4~ bes8 c4 d8 |
  ees8 bes4~ bes8 c[ ces] |
  bes bes c bes4 bes8 |
  bes4.( des) |
  
  c4. c4 d8 |
  ees bes4~ bes8 c ces |
  bes bes c bes4 aes8 |
  g4.~ g \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8[ ees] ees bes4 bes8 |
  << {ees ees4~ ees4.} {s2 s8 \teeny ees}>> \normalsize |
  aes8 aes aes aes4 aes8 |
  << {ees4.~ ees} {s2 s8 \teeny ees}>> \normalsize |
  
  f4~ f8 f[ f] f |
  << {bes, bes4~ bes4.} {s2 s8 \teeny bes}>> \normalsize |
  bes[ bes] d f4 f8 |
  bes4.~ bes\fermata |
  
  ees,4~ ees8 aes4 aes8 |
  ees g4~ g8 aes[ aes] |
  bes g aes bes4 aes8 |
  g4.~ g |
  
  aes4. aes4 aes8 |
  ees g4~ g8 aes[ aes] |
  bes g aes bes4 bes,8 |
  ees4.~ ees \bar"|."
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
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Boating Song"}}
  tagline = ""
}}


